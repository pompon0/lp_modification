//#define DEBUG_MODE
//#define VERBOSE
//#define PROFILE

#include <iostream>
#include <algorithm>
#include <random>
#include "utils/ctx.h"
#include "utils/log.h"
#include "utils/types.h"
#include "utils/read_file.h"
#include "lazyparam_prover/controller/prover.h"
#include "lazyparam_prover/controller/features.h"
#include "ffprover/xgboost.h"
#include "ffprover/search.h"
#include "ffprover/full_search.h"
#include "ffprover/tree.h"
#include "ffprover/mcts.pb.h"

#include "absl/flags/flag.h"
#include "absl/flags/parse.h"
#include "absl/time/clock.h"
#include "absl/time/time.h"

std::mt19937 rnd(7987345);
//std::uniform_real_distribution dist(0.,1.);

using namespace ff;

//ABSL_FLAG(absl::Duration,timeout,absl::Seconds(60),"spend timeout+eps time on searching");
//ABSL_FLAG(uint64_t,max_mem,3000000,"memory limit in bytes");
//ABSL_FLAG(uint64_t,max_infs,20000000,"limit on number of inferences");
//ABSL_FLAG(uint64_t,playout_depth,100,"limit on depth of a single playout");
//ABSL_FLAG(uint64_t,max_bigsteps,100,"limit on number of big steps");

//ABSL_FLAG(str,problem_path,"","path to TPTP problem");
//ABSL_FLAG(str,priority_model_path,"","path to priority model");
//ABSL_FLAG(str,reward_model_path,"","path to reward model");
//ABSL_FLAG(str,priority_training_path,"","output path for priority training data");
//ABSL_FLAG(str,reward_training_path,"","output path for reward training data");
//ABSL_FLAG(size_t,features_space_size,1<<15,"size of the space that features will be hashed into. Should be <50k");
//ABSL_FLAG(bool,full_search,false,"perform full DFS search, rather than MCTS search");

INL static inline double logistic(double v){ return 1./(1.+exp(-v)); }

ptr<mcts::Output> save_result(controller::Prover &prover, Result res) { FRAME("save_result");
  bool won = res.status==Result::SOLVED;
  auto output = collect_output(res.node,prover,won?1.:0.);
  auto proto = own(new mcts::Output());
  if(won) proto->mutable_priority()->set_libsvm(output->priority_data.show());
  proto->mutable_reward()->set_libsvm(output->reward_data.show());
  return proto;
}

template<typename Proto> inline static Proto proto_from_raw(const str &file_raw) { FRAME("proto_from_raw()");
  PROF_CYCLES("proto_from_raw");
  Proto proto;
  auto stream = new google::protobuf::io::CodedInputStream((const uint8_t*)(&file_raw[0]),file_raw.size());
  stream->SetRecursionLimit(100000000);
  if(!proto.ParseFromCodedStream(stream)) {
    error("failed to parse input");
  }
  return proto;
}

static absl::Duration proto_to_absl(const google::protobuf::Duration &proto) {
  return absl::Seconds(proto.seconds()) + absl::Nanoseconds(proto.nanos());
}

int main(int argc, char **argv) { 
  StreamLogger l(std::cerr); FRAME("main");
  std::ios::sync_with_stdio(0);
  
  str input_raw((std::istreambuf_iterator<char>(std::cin)), (std::istreambuf_iterator<char>()));
  auto input = proto_from_raw<mcts::Input>(input_raw);
  
  ptr<Model> priority_model;
  ptr<Model> reward_model;

  Search::Config cfg {
    .one_expansion_per_playout = false,
    .playouts_per_bigstep = 10000,
    .playout_depth = 20,
    .base_reward = [&](features::StateVec f) {
      //const auto value_factor = 0.3;
      return logistic(reward_model ? reward_model->predict(state_features(f)) : logit(1./f.goal_count));
    },
    .base_priority = [&](features::ActionVec f) {
      return priority_model ? exp(priority_model->predict(action_features(f))) : 1.;
    },
    .child_priority = [&](Tree::Ptr t, size_t i) {
      //return dist(rnd) * t.child(i).priority();

      double sum_visits = std::max<size_t>(1,t.visits());
      double visits = std::max<size_t>(1,t.child(i).visits());
      //double factor = sqrt(sum_visits/visits); // UCB no logarithm *)
      //double factor = sqrt(sum_visits)/visits; // PUCB from Alpha Zero *)
      double factor = sqrt(log(sum_visits)/visits); // Original Csaba Szepesvari *)
      const double ucb_const = 1.;
      return (t.child(i).rewards()/visits)
        + ucb_const*t.child(i).priority()*factor;
    },
    .bigstep_selector = [&](Tree::Ptr t) {
      const auto navigate_to_win = true; 
      if(auto w = t.won_depth(); navigate_to_win && w) {
        for(size_t i=0; i<t.child_count(); i++) {
          if(auto w2 = t.child(i).won_depth(); w2 && w2.get()==w.get()-1) return i;
        }
        error("inconsistent win_depth");
      }
      ssize_t max_i = -1;
      double max_p = -1;
      for(size_t i=0; i<t.child_count(); i++) {
        auto visits = t.child(i).visits();
        if(visits==0) continue;
        // choose by visits (+rewards tie breaker)
        double p = double(visits) + t.child(i).rewards()/double(visits);
        if(p>max_p){ max_p = p; max_i = i; }
      }
      DEBUG if(max_i==-1) error("no canididate for bigstep");
      return size_t(max_i);
    }
  };

  info("parsed");
  auto ctx = Ctx::with_timeout(Ctx::background(),proto_to_absl(input.timeout()));
  auto problem = controller::Problem::New(input.problem());
  auto prover = controller::Prover::New(problem,input.features_space_size());
  info("loaded problem");
  if(input.has_priority()) priority_model = Model::New(input.priority());
  if(input.has_reward()) reward_model = Model::New(input.reward());
  info("loaded models");

  auto tree = Tree::New();

  auto t0 = realtime_sec();
  auto cpu0 = cpu_proc_time_sec();
  auto cycles0 = __rdtsc();

  Result res = input.full_search() ?
    FullSearch{}.run(ctx,tree->root(),*prover) :
    Search(cfg).run(ctx,tree->root(),*prover);
  
  auto t1 = realtime_sec();
  auto cpu1 = cpu_proc_time_sec();
  auto cycles1 = __rdtsc();
  
  prover = controller::Prover::New(problem,input.features_space_size());
  auto out = save_result(*prover,res);

  out->set_status([&]()INLL{
    switch(res.status) {
    case Result::SOLVED: return mcts::THEOREM;
    case Result::DEADEND: return mcts::DEADEND;
    case Result::CANCELLED: return mcts::RESOURCE_OUT;
    default: error("search.run() = %",res.status);
    }
  }());
  
  for(auto &[n,s] : profile.scopes){
    auto *e = out->mutable_profiler()->add_entries();
    e->set_label(n);
    e->set_count(s.count);
    e->set_cycles(s.cycles);
    e->set_time_s(s.time);
  };
  
  auto *s = out->mutable_stats();
  s->set_bigsteps(res.stats.bigsteps);
  s->set_playouts(res.stats.playouts);
  s->set_inferences(res.stats.inferences);
  s->set_realtime_sec(t1-t0);
  s->set_cpu_sec(cpu1-cpu0);
  s->set_cpu_cycles(cycles1-cycles0);

  if(!out->SerializeToOstream(&std::cout)) {
    error("outProto.SerializeToOstream() failed");  
  }
  return 0;
}
