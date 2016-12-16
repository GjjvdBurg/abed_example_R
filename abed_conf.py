import sklearn.metrics

##############################################################################
#                                General Settings                            #
##############################################################################
PROJECT_NAME = 'abed_example_R'
TASK_FILE = 'abed_tasks.txt'
AUTO_FILE = 'abed_auto.txt'
RESULT_DIR = './results'
STAGE_DIR = './stagedir'
MAX_FILES = 1000
ZIP_DIR = './zips'
LOG_DIR = './logs'
OUTPUT_DIR = './output'
AUTO_SLEEP = 120
HTML_PORT = 8000
COMPRESSION = 'bzip2'

##############################################################################
#                          Server parameters and settings                    #
##############################################################################
REMOTE_USER = 'username'
REMOTE_HOST = 'address.of.host'
REMOTE_DIR = '/home/%s/projects/%s' % (REMOTE_USER, PROJECT_NAME)
REMOTE_PORT = 22
REMOTE_SCRATCH = None
REMOTE_SCRATCH_ENV = 'TMPDIR'

##############################################################################
#                      Settings for Master/Worker program                    #
##############################################################################
MW_SENDATONCE = 20 # number of tasks (hashes!) to send at once
MW_COPY_WORKER = False
MW_COPY_SLEEP = 120

##############################################################################
#                               Experiment type                              #
##############################################################################
# Uncomment the desired type
# Model assessment #
#TYPE = 'ASSESS'

# Cross validation with train and test dataset #
TYPE = 'CV_TT'
CV_BASESEED = 123456
YTRAIN_LABEL = 'y_train'

# Commands defined in a text file #
#TYPE = 'RAW'
#RAW_CMD_FILE = '/path/to/file.txt'

##############################################################################
#                                Build settings                              #
##############################################################################
NEEDS_BUILD = False    # If remote compilation is required
BUILD_DIR = 'build'    # Relative directory where build takes place
BUILD_CMD = 'make all' # Build command

##############################################################################
#                      Experiment parameters and settings                    #
##############################################################################
DATADIR = 'datasets'
EXECDIR = 'execs'

DATASETS = [('dataset_%i_train' % i, 'dataset_%i_test' % i) for i in 
        range(1, 11)]
DATASET_NAMES = {k:str(i) for i, k in enumerate(DATASETS)}

METHODS = ['OLS', 'Lasso', 'Ridge']
PARAMS = {
        'OLS': {},
        'Lasso': {
            'alpha': [pow(2, x) for x in range(-8, 9, 2)]
            },
        'Ridge': {
            'alpha': [pow(2, x) for x in range(-8, 9, 2)]
            }
        }

COMMANDS = {
        'OLS': ("Rscript --no-save --slave {execdir}/ols.R "
            "{datadir}/{train_dataset}.RData {datadir}/{test_dataset}.RData"),
        'Lasso': ("Rscript --no-save --slave {execdir}/lasso.R "
            "{datadir}/{train_dataset}.RData {datadir}/{test_dataset}.RData "
            "{cv_seed} {alpha}"),
        'Ridge': ("Rscript --no-save --slave {execdir}/ridge.R "
            "{datadir}/{train_dataset}.RData {datadir}/{test_dataset}.RData "
            "{cv_seed} {alpha}")
        }

METRICS = {
        'MSE': {
            'metric': sklearn.metrics.mean_squared_error,
            'best': min
            },
        'MAE': {
            'metric': sklearn.metrics.mean_absolute_error,
            'best': min
            }
        }

SCALARS = {
        'time': {
            'best': min
            },
        }

RESULT_PRECISION = 4

DATA_DESCRIPTION_CSV = None

REFERENCE_METHOD = None

SIGNIFICANCE_LEVEL = 0.05

###############################################################################
#                                PBS Settings                                 #
###############################################################################
PBS_NODES = 1
PBS_WALLTIME = 60   # Walltime in minutes
PBS_CPUTYPE = None
PBS_CORETYPE = None
PBS_PPN = None
PBS_MODULES = ['mpicopy', 'python/2.7.9']
PBS_EXPORTS = ['PATH=$PATH:/home/%s/.local/bin/abed' % REMOTE_USER]
PBS_MPICOPY = ['datasets', 'execs', TASK_FILE]
PBS_TIME_REDUCE = 600 # Reduction of runtime in seconds

