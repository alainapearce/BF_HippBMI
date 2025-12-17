function cat12_6_ROIextract(TPMname, atlas, surf, subset);
    %
    % This function was written by Alaina Pearce in the Spring of 2020 to
    % compile extracted ROI values for a specific atlas accross 
    % participants. This function is written to be compile QC across a 
    % group of subjects. This will either process all participants for the 
    % entered tissue probability map or parIDs listed in subset file
    % 
    % TPMname: TPM name 
    % surf: "no" or "yes"
    % subset: prefix of folder and file name with subset of ids
    % (prefix/Data/prefix_parlist.csv)
    % atlas: neuromorphometrics or cobra
    
    % ==========================================================================================================
    %                                          Preludes: settings, inputs, etc.
    % ==========================================================================================================
    % suppress warning    
    warning('off','MATLAB:prnRenderer:opengl');
    warning('off', 'MATLAB:hg:AutoSoftwareOpenGL');
    
    %for debugging
    %TPM_name = 'all';
    
    %set initial path structures
    %!need to edit this section (and all paths) if move script or any directories it calls!

    %get working directory path for where this script is saved
    %(individual path info '/Box Sync/b-childfoodlab Shared/MRIstruct/Scripts')
    script_wd = mfilename('fullpath');

%    if ismac()
%        slash = '/';
%    else 
%        slash = '\';
%    end
    
    %for us on the computing cluster:
    slash = '/';

    %get location/character number for '/" in file path
    slashloc_wd=find(script_wd==slash);
    
    %use all characters in path name upto the second to last slash (individual path info
    %'/Box Sync/b-childfoodlab Shared/RO1_Brain_Mechanisms_IRB_5357/MRIstruct/)
    base_wd = script_wd(1:slashloc_wd(end-1));

    %this will tell matlab to look at all files withing the base_wd/CAT--so any
    %subfolder will be added to search path
    result_main_folder=[base_wd slash 'CAT' slash 'ProcessedData'];
    

    if exist('subset', 'var')
        subset_uscore = ['_' char(subset)];
    else
        subset_uscore = '';
    end

    % ==========================================================================================================
    %                                          subcortical
    % ==========================================================================================================
    
    % check if already processed
    if exist([result_main_folder slash 'TPM' char(TPMname) char(subset_uscore) '_ROIdat' slash 'TPM' char(TPMname) char(subset_uscore) '_ROI_dat_' char(atlas) '.csv'], 'file')
        % subject complete
        disp(' ');
        disp(['  ... ROI statistic have already been saved for the tissue probabiity map: TPM' char(TPMname) char(subset_uscore)]);
    else     
        % start working on the subject
        disp(' ');
        disp(['  ... Compiling ROI statistics across participants for tissue probabiity map: TPM' char(TPMname) char(subset_uscore)]);

        %make QC directory
        if ~exist([result_main_folder slash 'TPM' char(TPMname) char(subset_uscore) '_ROIdat'], 'dir')
            mkdir([result_main_folder slash 'TPM' char(TPMname) char(subset_uscore) '_ROIdat']);
        end
        
        % ==========================================================================================================
        %                                          loop
        % ==========================================================================================================
        % get all participant ROI subcortical files and concatonate for full path and
        % name
        par_ROI = struct2table(dir([result_main_folder slash '*' char(TPMname) slash 'label' slash 'catROI_*_T1.mat']));
        par_ROI.fullpath = strcat(par_ROI.folder, slash, par_ROI.name, ',1');
        
        %based results folder for participant
        results_string = @(v) v(1:(find('/' == v, 1, 'last'))-1);
        par_ROI.par_base_results = arrayfun(@(S) results_string(char(S)), par_ROI.folder, 'UniformOutput', false);

        %folder name for participant
        par_string = @(v, x) v((find('/' == x, 1, 'last'))+1:(find('/' == v, 1, 'last'))-1);
        par_ROI.par_name = arrayfun(@(S, T) par_string(char(S), char(T)), par_ROI.folder, par_ROI.par_base_results,'UniformOutput', false);
        
        %parID
        parID_string = @(v) v(1:(find('_' == v, 1, 'last'))-1);
        par_ROI.parID = arrayfun(@(S) parID_string(char(S)), par_ROI.par_name, 'UniformOutput', false);
        
        %reports path
        report_string = @(v, w) [v slash 'report' slash 'cat_' w '_T1.xml'];
        par_ROI.reports_path = arrayfun(@(S, T) report_string(char(S), char(T)), par_ROI.par_base_results, par_ROI.parID, 'UniformOutput', false);
        
        % ==========================================================================================================
        %                                          Subset if needed
        % ==========================================================================================================

        if exist('subset', 'var')
            subset_tab = readtable([base_wd slash 'CAT' slash char(subset) slash 'Data' slash char(subset) '_parlist.csv'], 'Delimiter', ',');

            %subset table to just include subset IDs
            par_ROI = par_ROI(ismember(par_ROI.parID, subset_tab.parID), :);
        end

        % ==========================================================================================================
        %                                          loop
        % ==========================================================================================================

        %loop through to get participant intercranial volume estimates
        %need this in same order as the T1

        for p=1:height(par_ROI)
            par_catROI = load([char(par_ROI.par_base_results(p)) slash 'label' slash 'catROI_' char(par_ROI.parID(p)) '_T1.mat']);
            par_catROI_atlas = getfield(par_catROI.S, char(atlas));
            
            par_catROI_table = table(repmat(par_ROI.parID(p), length(par_catROI_atlas.ids), 1));
            par_catROI_table.Properties.VariableNames = {'parID'};
            
            par_catROI_table.atlas(:) = cellstr(atlas);
            par_catROI_table.version(:) = cellstr(par_catROI_atlas.version);
            
            par_catROI_table.ids = par_catROI_atlas.ids;
            par_catROI_table.region = par_catROI_atlas.names;
            par_catROI_table.vgm = par_catROI_atlas.data.Vgm;

            if strcmp(char(atlas), 'cobra')
                par_catROI_table.vwm = par_catROI_atlas.data.Vwm;
            elseif strcmp(char(atlas), 'neuromorphometrics')
                par_catROI_table.vgm = par_catROI_atlas.data.Vgm;
            end
            
            if p == 1
                catROI_table = par_catROI_table;
            else
                catROI_table = vertcat(catROI_table, par_catROI_table);
            end
            
            writetable(par_catROI_table, [char(par_ROI.par_base_results(p)) slash 'label' slash 'catROI_' char(par_ROI.parID(p)) '_' char(atlas) '_T1.csv'], 'Delimiter', ',');
        end
        
        writetable(catROI_table, [result_main_folder slash 'TPM' char(TPMname) char(subset_uscore) '_ROIdat' slash 'TPM' char(TPMname) char(subset_uscore) '_ROI_dat_' char(atlas) '.csv'], 'Delimiter', ',');
      
    end   
    
    % ==========================================================================================================
    %                                          surface
    % ==========================================================================================================
    if strcmp(surf, 'yes')
        if exist([result_main_folder slash 'TPM' char(TPMname) char(subset_uscore) '_ROIdat' slash 'TPM' char(TPMname) char(subset_uscore) '_ROIs_dat_HCP.csv'], 'file')
            % subject complete
            disp(' ');
            disp(['  ... ROI surface statistics have already been saved for the tissue probabiity map: TPM' char(TPMname) char(subset_uscore)]);
        else    
            
            % start working on the subject
            disp(' ');
            disp(['  ... Compiling ROI statistics across participants for tissue probabiity map: TPM' char(TPMname) char(subset_uscore)]);

            %make QC directory
            if ~exist([result_main_folder slash 'TPM' char(TPMname) char(subset_uscore) '_ROIdat'], 'dir')
                mkdir([result_main_folder slash 'TPM' char(TPMname) char(subset_uscore) '_ROIdat']);
            end

            % get all participant ROI surface files and concatonate for full path and
            par_ROI_surf = struct2table(dir([result_main_folder slash '*' char(TPMname) slash 'label' slash 'catROIs_*_T1.mat']));
            par_ROI_surf.fullpath = strcat(par_ROI_surf.folder, slash, par_ROI_surf.name, ',1');
            
            %based results folder for participant
            results_string = @(v) v(1:(find('/' == v, 1, 'last'))-1);
            par_ROI_surf.par_base_results = arrayfun(@(S) results_string(char(S)), par_ROI_surf.folder, 'UniformOutput', false);

            %folder name for participant
            par_string = @(v, x) v((find('/' == x, 1, 'last'))+1:(find('/' == v, 1, 'last'))-1);
            par_ROI_surf.par_name = arrayfun(@(S, T) par_string(char(S), char(T)), par_ROI_surf.folder, par_ROI_surf.par_base_results,'UniformOutput', false);

            %parID
            parID_string = @(v) v(1:(find('_' == v, 1, 'last'))-1);
            par_ROI_surf.parID = arrayfun(@(S) parID_string(char(S)), par_ROI_surf.par_name, 'UniformOutput', false);

            % ==========================================================================================================
            %                                          Subset if needed
            % ==========================================================================================================

            if exist('subset', 'var')
                subset_tab = readtable([base_wd slash 'CAT' slash char(subset) slash 'Data' slash char(subset) '_parlist.csv'], 'Delimiter', ',');

                %subset table to just include subset IDs
                par_ROI_surf = par_ROI_surf(ismember(par_ROI_surf.parID, subset_tab.parID), :);
            end
            
            % ==========================================================================================================
            %                                          loop
            % ==========================================================================================================
           
            for p=1:height(par_ROI_surf)
                par_catROI_surf = load([char(par_ROI_surf.par_base_results(p)) slash 'label' slash 'catROIs_' char(par_ROI_surf.parID(p)) '_T1.mat']);
                par_catROI_surf_atlas = getfield(par_catROI_surf.S, 'aparc_HCP_MMP1');

                par_catROI_surf_table = table(repmat(par_ROI_surf.parID(p), length(par_catROI_surf_atlas.ids), 1));
                par_catROI_surf_table.Properties.VariableNames = {'parID'};

                par_catROI_surf_table.atlas(:) = cellstr('aparc_HCP_MMP1');
                par_catROI_surf_table.version(:) = cellstr(par_catROI_surf_atlas.version);

                par_catROI_surf_table.ids = par_catROI_surf_atlas.ids;
                par_catROI_surf_table.region = par_catROI_surf_atlas.names;
                par_catROI_surf_table.thickness = par_catROI_surf_atlas.data.thickness';
                

                if p == 1
                    catROI_surf_table = par_catROI_surf_table;
                else
                    catROI_surf_table = vertcat(catROI_surf_table, par_catROI_surf_table);
                end

                writetable(par_catROI_surf_table, [char(par_ROI_surf.par_base_results(p)) slash 'label' slash 'catROIs_' char(par_ROI_surf.parID(p)) '_HCP_T1.csv'], 'Delimiter', ',');
            end
            
            
            writetable(catROI_surf_table, [result_main_folder slash 'TPM' char(TPMname) char(subset_uscore) '_ROIdat' slash 'TPM' char(TPMname) char(subset_uscore) '_ROIs_dat_HCP.csv'], 'Delimiter', ',');

        end
        
        
        % ==========================================================================================================
        %                                          Housekeeping
        % ==========================================================================================================

        % say goodbye
        disp(['   ... thank you for using this script and']);
        disp(['=== Have a nice day ===']);
        disp(' ');
    end

    % reset data format
    format;
    return;
end
