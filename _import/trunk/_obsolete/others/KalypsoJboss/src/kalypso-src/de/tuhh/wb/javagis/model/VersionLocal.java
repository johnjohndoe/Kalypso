package de.tuhh.wb.javagis.model;



public interface VersionLocal
   extends javax.ejb.EJBLocalObject
{
   public void setId(java.lang.Integer objectId);
   public java.lang.Integer getId();

   public void setVersionProject(java.lang.String project);
   public java.lang.String getVersionProject();

   public void setVersionState(java.lang.String state);
   public java.lang.String getVersionState();

   public void setVersionName(java.lang.String name);
   public java.lang.String getVersionName();

   public void setVersionDescription(java.lang.String description);
   public java.lang.String getVersionDescription();

   public void setVersionHistory(java.lang.String history);
   public java.lang.String getVersionHistory();

   public int getNumberOfObjects();
}
