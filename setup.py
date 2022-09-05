from setuptools import find_packages, setup
import codecs
import os.path

with open('README.md', 'r', encoding='utf-8') as fh:
    long_description = fh.read()

def get_data_files():
    data_files = [
        ('share/doc/difftime', ['README.md', 'LICENSE']),
    ]

    return data_files

def read(rel_path):
    here = os.path.abspath(os.path.dirname(__file__))
    with codecs.open(os.path.join(here, rel_path), 'r') as fp:
        return fp.read()

def get_version(rel_path):
    ''' Single source package version in src/package_name/__init__.py '''
    for line in read(rel_path).splitlines():
        if line.startswith('__version__'):
            delim = '"' if '"' in line else "'"
            return line.split(delim)[1]
    else:
        raise RuntimeError("Unable to find version string.")

setup(
    name='difftime',
    version=get_version("src/difftime/__init__.py"),
    license='GPL-3.0',
    author='Joseph Diza',
    author_email='josephm.diza@gmail.com',
    description='Calculate differences in time intervals',
    long_description=long_description,
    long_description_content_type='text/markdown',
    url='https://github.com/jmdaemon/difftime',
    data_files=get_data_files(),
    project_urls={
        'Bug Tracker': 'https://github.com/jmdaemon/difftime/issues',
    },
    classifiers=[
        'Programming Language :: Python :: 3',
        'Operating System :: OS Independent'
    ],
    package_dir={'': 'src'},
    packages=find_packages(where='src'),
    python_requires='>=3.10',
    install_requires=[],
    entry_points={
        'console_scripts': [
            'difftime = difftime.difftime:main',
        ],
    },
    test_suite='tests',
)
