FROM jenkins/jenkins:lts
ENV JAVA_OPTS="-Djenkins.install.runSetupWizard=false -Dpermissive-script-security.enabled=true"
COPY plugins.txt /usr/share/jenkins/ref/plugins.txt

RUN jenkins-plugin-cli --plugin-file /usr/share/jenkins/ref/plugins.txt
RUN chown -R jenkins:jenkins /var/jenkins_home
RUN for f in /usr/share/jenkins/ref/plugins/*.jpi; do mv "$f" "$f.override"; done
EXPOSE 8080 50000