package de.tuhh.wb.javagis.data;
import java.io.File;
import java.util.Vector;
public interface  VersionAccess
{
    /**
     * returns the number of available versions
     */
    public int getSize();

    public Vector getThemeKeys();

    public String getProjectName(int position);
    public void   setProjectName(int position,String projectName);
    public String getProjectName(String themeKey,Object vId);

    public String getThemeName(int position);
    public String getThemeKey(int position);

    // no set

    public String getState(int position);
    public void   setState(int position,String state);
    public String getState(String themeKey,Object vId);

    public String getName(int position);
    public void   setName(int position,String name);
    public String getName(String themeKey,Object vId);
    public String getFullName(String themeKey,Object vId);
    public String getDescription(int position);
    public void   setDescription(int position,String description);
    public String getDescription(String themeKey,Object vId);

    public String getHistory(int position);
    // no set

    public void updateIndex();
    
    public void createVersion(String projectName,String themeKey,String state,String name, String description);
    public void createExtendedVersion(int positionOfBaseversion,String projectName,String themeKey,String state,String name, String description);

    public void renameVersion(String themeKey,Object vId,String project,String state,String name,String description);
    public void copyVersion(String themeKey,Object vId,String project,String state,String name,String description);


    public void removeVersion(int position);

    //    public void openVersion(int pos);
    public Version getVersion(int pos);

    public Object getVersionId(int pos);

 
    public void openObjectTableView(int pos);
    public void openRelationTableView(int pos);

    public void openNetView(int pos);

    //    public void xmlImport(int pos,String fileName);
    public void xmlImport(String themeKey,Object vId,File importFile);

    //    public void xmlExport(int pos);
    public void xmlExport(String themeKey,Object vId,File exportFile);

    public void simulate(int pos);
    
}
