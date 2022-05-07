from setuptools import find_packages, setup

with open('README.md', 'r', encoding='utf-8') as fh:
    long_description = fh.read()

def get_data_files():
    data_files = [
        ('share/doc/difftime', ['README.md', 'LICENSE']),
    ]

    return data_files

setup(
    name='difftime',
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
    py_modules=['difftime'],
    install_requires=[],
    entry_points={
        'console_scripts': [
            'difftime = difftime.difftime:main',
        ],
    },
    test_suite='tests',
)
