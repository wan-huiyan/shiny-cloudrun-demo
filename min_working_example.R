library(googleCloudRunner)

#cr_setup()
#cr_setup_test()

cr_project_set(projectId = "mightyhive-data-science-poc")

# a repo with the Dockerfile template
repo <- cr_buildtrigger_repo("wan-huiyan/shiny-cloudrun-demo")

# deploy a cloud build trigger so each commit build the image
cr_deploy_docker_trigger(repo,
                         image = "shiny-cloudrun")

# deploy to Cloud Run
cr_run(sprintf("gcr.io/%s/shiny-cloudrun:latest",cr_project_get()),
       name = "shiny-cloudrun",
       concurrency = 80,
       max_instances = 1)
