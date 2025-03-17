import javaposse.jobdsl.dsl.*
import javaposse.jobdsl.dsl.DslFactory

// Create the Tools folder
folder('Tools') {
    description('Folder for miscellaneous tools.')
}

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
            external('jobs.groovy')
        }
    }

    triggers {
    }
    folder('Tools') {
        description('Folder for miscellaneous tools.')
    }

}