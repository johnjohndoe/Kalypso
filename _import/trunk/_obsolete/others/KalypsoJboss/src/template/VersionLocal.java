package de.tuhh.wb.javagis.model;

import java.lang.*;
import javax.ejb.CreateException;
import javax.ejb.EntityBean;
import javax.ejb.EntityContext;
import javax.ejb.FinderException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;


public interface VersionLocal
   extends javax.ejb.EJBLocalObject
{
   public void setId(java.lang.Integer objectId);
   public java.lang.Integer getId();

   public void setName(java.lang.String name);
   public java.lang.String getName();

   public void setState(java.lang.String state);
   public java.lang.String getState();

   public void setComment(java.lang.String comment);
   public java.lang.String getComment();

   public int getNumberOfObjects();
}
