#This script can be used to combine files produced by AnimalTA
Main_folder="G:/AnimalTA_users/TEBIS/All_projects"

#We first identify all the projects folders:
all_projects <- basename(list.dirs(Main_folder, recursive = FALSE))

Combined_data_trajectories=NULL
Combined_data_morpho=NULL

#We combine all projects together:
for (project in all_projects)
{
  path_project=paste(Main_folder,project,"Results",sep="/")
  
  #Trajectory data
  path_file=paste(path_project,"Results_by_ind.csv",sep="/")
  data_curr_project=read.table(path_file,sep=";",h =T)
  data_curr_project=cbind(Groupe=substring(project,16,1000),data_curr_project)
  Combined_data_trajectories=rbind(Combined_data_trajectories,data_curr_project)
  
  
  #Morpho data
  path_file=paste(path_project,"Morphometrics.csv",sep="/")
  data_curr_project=read.table(path_file,sep=";",h =T)
  data_curr_project=cbind(Groupe=substring(project,16,1000),data_curr_project)
  Combined_data_morpho=rbind(Combined_data_morpho,data_curr_project)
  
}











