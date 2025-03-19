#FROM jenkins/jenkins:lts
#COPY plugins.txt /usr/share/jenkins/ref/plugins.txt
#
#RUN jenkins-plugin-cli --plugin-file /usr/share/jenkins/ref/plugins.txt
#RUN chown -R jenkins:jenkins /var/jenkins_home
#EXPOSE 8080 50000
FROM jenkins/jenkins:latest

USER root

RUN apt-get update -y \
    && apt-get upgrade -y \
    && apt-get install -y build-essential \
    && rm -rf /var/lib/apt/lists/*  # Cleanup to reduce image size

USER jenkins

RUN jenkins-plugin-cli --plugins \
    cloudbees-folder \
    configuration-as-code \
    credentials \
    github \
    instance-identity \
    job-dsl \
    script-security \
    structs \
    role-strategy \
    ws-cleanup

EXPOSE 8080 50000
