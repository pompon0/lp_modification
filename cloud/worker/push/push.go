package push

import (
  "context"
  "errors"
  "fmt"
  "log"
  //"strings"

  "google.golang.org/api/option"
  "google.golang.org/api/run/v1"
  "google.golang.org/api/googleapi"
  "github.com/google/go-containerregistry/pkg/authn"
  "github.com/google/go-containerregistry/pkg/name"
  "github.com/google/go-containerregistry/pkg/v1/remote"
  "github.com/google/go-containerregistry/pkg/v1/remote/transport"
  "github.com/google/go-containerregistry/pkg/v1/tarball"
  "github.com/pompon0/tptp_benchmark_go/utils"
)

const endpoint = "https://us-central1-run.googleapis.com"
const projectName = "namespaces/mapstore-201213"
const serviceName = "namespaces/mapstore-201213/services/worker"

const image = "__main__/cloud/worker/worker_img.tar"
const reference = "gcr.io/mapstore-201213/worker"

func Push(ctx context.Context, commit string) error {
  // upload image
  img, err := tarball.ImageFromPath(utils.Runfile(image), nil)
  if err!=nil { return fmt.Errorf("tarball.ImageFromPath(): %v",err) }
  digest, err := img.Digest()
  if err!=nil { return fmt.Errorf("img.Digest(): %v",err) }
  digestRef, err := name.NewDigest(fmt.Sprintf("%s@%s", reference, digest))
  if err!=nil { return fmt.Errorf("name.NewDigest(): %v",err) }
  remoteImg, err := remote.Image(digestRef, remote.WithAuthFromKeychain(authn.DefaultKeychain))
  if err!=nil {
    var tErr *transport.Error
    if errors.As(err,&tErr) && tErr.StatusCode==404 {
      remoteImg,err = nil,nil
    } else {
      return fmt.Errorf("remote.Image(): %v",err)
    }
  }
  if remoteImg == nil {
    log.Printf("image not found, uploading...")
    if err := remote.Write(digestRef,img,remote.WithAuthFromKeychain(authn.DefaultKeychain)); err!=nil {
      return fmt.Errorf("remote.Write(): %v",err)
    }
  } else {
    log.Printf("image found")
  }

  // deploy new revision
  log.Printf("deploying new revision...")
  runService, err := run.NewService(ctx,option.WithEndpoint(endpoint))
  if err!=nil { return fmt.Errorf("run.NewService()",err) }
  ns := run.NewNamespacesService(runService)

  template := &run.RevisionTemplate {
    Metadata: &run.ObjectMeta {
      Annotations: map[string]string {
        "autoscaling.knative.dev/maxScale": "1000",
      },
    },
    Spec: &run.RevisionSpec {
      ContainerConcurrency: 1,
      Containers: []*run.Container {{
        Image: digestRef.Name(),
        Env: []*run.EnvVar {{
          Name: "COMMIT",
          Value: commit,
        }},
        Ports: []*run.ContainerPort {{
          ContainerPort: 8080,
        }},
        Resources: &run.ResourceRequirements {
          Limits: map[string]string {
            "cpu": "1000m",
            "memory": "2048Mi",
          },
        },
      }},
      ServiceAccountName: "804779577777-compute@developer.gserviceaccount.com",
      TimeoutSeconds: 300,
    },
  }

  revisions,err := ns.Revisions.List(projectName).Context(ctx).Do()
  if err!=nil { return fmt.Errorf("ns.Revisions.List(%q): %+v",serviceName,err) }

  service,err := ns.Services.Get(serviceName).Context(ctx).Do()
  if err!=nil { return fmt.Errorf("ns.Service.Get(%q): %v",serviceName,err) }

  service.Spec.Template = template //.Metadata.Name = "worker-00051-fuc"
  service.Spec.Traffic = []*run.TrafficTarget{{
    LatestRevision: true,
    Percent: 100,
  }}
  service,err = ns.Services.ReplaceService(serviceName,service).Context(ctx).Do()
  if err!=nil {
    return fmt.Errorf("ns.Service.ReplaceService(%q): %v",serviceName,err)
  }

  // cleanup old revisions
  log.Printf("wiping out old revisions...")
  for _,r := range revisions.Items {
    name := projectName + "/revisions/" + r.Metadata.Name
    if _,err := ns.Revisions.Delete(name).Context(ctx).Do(); err!=nil {
      inUse := false
      for _,e := range err.(*googleapi.Error).Errors {
        if e.Reason == "failedPrecondition" {
          inUse = true
        }
      }
      if !inUse {
        return fmt.Errorf("Delete(%q): %v",name,err)
      }
    }
  }
  return nil
}
