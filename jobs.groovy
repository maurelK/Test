import javaposse.jobdsl.dsl.*

// Create the Tools folder
folder('Tools') {
    description('Folder for miscellaneous tools.')
}

// Create the clone-repository job
job('Tools/clone-repository') {
    description('Clones a Git repository.')
    parameters {
        stringParam('GIT_REPOSITORY_URL', '', 'Git URL of the repository to clone')
    }
    steps {
        shell('git clone $GIT_REPOSITORY_URL')
    }
    wrappers {
        preBuildCleanup()
    }
    triggers {
        manual()
    }
}

// Create the SEED job
job('Tools/SEED') {
    description('Creates a new job based on the provided GitHub repository and display name.')
    parameters {
        stringParam('GITHUB_NAME', '', 'GitHub repository owner/repo_name (e.g.: "EpitechIT31000/chocolatine")')
        stringParam('DISPLAY_NAME', '', 'Display name for the job')
    }
    steps {
        dsl {
            text('''
                job("${DISPLAY_NAME}") {
                    description("Job for ${GITHUB_NAME}")
                    properties {
                        githubProjectUrl("https://github.com/${GITHUB_NAME}")
                    }
                    scm {
                        git {
                            remote {
                                url("https://github.com/${GITHUB_NAME}.git")
                            }
                            branches('*/main')
                        }
                    }
                    triggers {
                        scm('H/1 * * * *')  // Poll SCM every minute
                    }
                    wrappers {
                        preBuildCleanup()
                    }
                    steps {
                        shell('make fclean')
                        shell('make')
                        shell('make tests_run')
                        shell('make clean')
                    }
                }
            ''')
        }
    }
}