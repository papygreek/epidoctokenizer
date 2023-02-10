from setuptools import setup
setup(
    name='epidoctokenizer',
    version='0.1',
    url='https://github.com/papygreek/epidoctokenizer',
    author='Erik Henriksson',
    author_email='erik.ilmari.henriksson@gmail.com',
    description='A tokenizer for EpiDoc XML documents',
    packages=['epidoctokenizer'],
    install_requires=['regex', 'lxml', 'python-dotenv'],
    zip_safe=False
)