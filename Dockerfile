FROM jenkins/jenkins:lts

COPY plugins.txt /usr/share/jenkins/ref/plugins.txt

RUN jenkins-plugin-cli --plugin-file /usr/share/jenkins/ref/plugins.txt

COPY ./job_dsl.groovy /var/jenkins_home/casc_configs/job_dsl.groovy

USER jenkins
EXPOSE 8080 50000
