# Copyright 2017 Observational Health Data Sciences and Informatics
#
# This file is part of MethodsLibraryPleEvaluation
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

# Format and check code
OhdsiRTools::formatRFolder()
OhdsiRTools::checkUsagePackage("MethodsLibraryPleEvaluation")
OhdsiRTools::updateCopyrightYearFolder()

# Create manual
shell("rm extras/MethodsLibraryPleEvaluation.pdf")
shell("R CMD Rd2pdf ./ --output=extras/MethodsLibraryPleEvaluation.pdf")

# Create analysis details
MethodsLibraryPleEvaluation::createCohortMethodSettings(fileName = "inst/settings/cmAnalysisSettings.txt")
MethodsLibraryPleEvaluation::createSccsSettings(fileName = "inst/settings/sccsAnalysisSettings.txt")
MethodsLibraryPleEvaluation::createSelfControlledCohortSettings(fileName = "inst/settings/sccAnalysisSettings.txt")
MethodsLibraryPleEvaluation::createIctpdSettings(fileName = "inst/settings/ictpdAnalysisSettings.txt")
MethodsLibraryPleEvaluation::createCaseControlSettings(fileName = "inst/settings/ccAnalysisSettings.txt")
MethodsLibraryPleEvaluation::createCaseCrossoverSettings(fileName = "inst/settings/ccrAnalysisSettings.txt")

# Insert cohort definitions into package
# OhdsiRTools::insertCirceDefinitionInPackage(2409, "Rheumatoid arthritis")
OhdsiRTools::insertCirceDefinitionInPackage(2542, "Osteoarthritis")

