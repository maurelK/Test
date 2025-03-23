folder('Tools') {
    displayName('Tools')
    description('Folder for miscellaneous tools.')

    job('Tools/clone-repository') {
        wrappers {
            preBuildCleanup()
        }
        parameters {
            stringParam('GIT_REPOSITORY_URL', '', 'Git URL of the repository to clone')
        }
        steps {
            shell('git clone $GIT_REPOSITORY_URL')
        }
    }

    job('Tools/SEED') {
        description('Job to create other jobs.')
        parameters {
            stringParam('GITHUB_NAME', '', 'GitHub repository owner/repo_name (e.g.: "EpitechIT31000/chocolatine")')
            stringParam('DISPLAY_NAME', '', 'Display name for the job')
        }
        triggers {
            cron('H/1 * * * *')
        }
        steps {
            dsl {
                text("""
                    job("\${DISPLAY_NAME}") {
                        description("Job created by SEED job")
                        scm {
                            git {
                                remote {
                                    url("https://github.com/\${GITHUB_NAME}.git")
                                }
                            }
                        }
                        triggers {
                            scm('H/1 * * * *')
                        }
                        steps {
                            shell('make fclean')
                            shell('make')
                            shell('make tests_run')
                            shell('make clean')
                        }
                        cleanBeforeBuild(true)
                    }
                """)
            }
        }
    }
}
