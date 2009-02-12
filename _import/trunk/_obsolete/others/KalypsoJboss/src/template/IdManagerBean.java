package de.tuhh.wb.javagis.model;
/**
 * java-code for BEAN:
 *
 * Id-Manager
 *
 */

import javax.ejb.CreateException;
import javax.ejb.EntityBean;
import javax.ejb.EntityContext;
import javax.ejb.FinderException;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;  


/**
 * 
 * @ejb:bean   name="IdManager"
 *             jndi-name="ejb/IdManager"
 *             local-jndi-name="ejb/IdManager"
 *             view-type="local"
 *             type="CMP"
 *             cmp-version="2.x"
 *             primkey-field="tableName"
 * @ejb:pk generate="false" class="java.lang.String"
 */
public abstract class IdManagerBean implements EntityBean
{
    private transient EntityContext ctx;
    
    /**
     * @ejb:interface-methode
     * @ejb:persistent-field
     */      
    public abstract void setTableName(java.lang.String tableName);

    /**
     * @ejb:interface-methode
     * @ejb:persistent-field
     */
    public abstract java.lang.String getTableName();
    
    // ejb:pk-field
    
    /**
     * @ejb:interface-methode
     * @ejb:persistent-field
     */
    public abstract void setMaxId(Integer maxId);
    
    /**
     * @ejb:interface-methode
     * @ejb:persistent-field
     */
    public abstract Integer getMaxId();
    
    /**
     * @ejb:interface-method
     */
    public Integer useId()
    {
	Integer iD=getMaxId();
	setMaxId(new Integer(iD.intValue()+1));
	System.out.println("IdManagerBean: generated ID #"+iD.intValue()+" for table \""+getTableName()+"\"");
	return iD;
    }

    /**
     * @ejb:create-method
     */
    public String ejbCreate(String tableName) throws CreateException
    {
	if(tableName == null)  throw new CreateException("tableName is null :-(");
	
	this.setTableName(tableName);
	this.setMaxId(new Integer(1));
	return null;
    }
    
    public void ejbPostCreate(String tableName)
    {
	System.out.println("IdManager: created IdManagerEntity for \""+tableName+"\"-Table.");
    }
    
    // Methoden des Entity-Bean-Interface:
    public void ejbActivate()
    {}

    public void ejbPassivate()
    {}
    
    public void setEntityContext(EntityContext ctx)
    {
	this.ctx=ctx;
    }
    
    public void unsetEntityContext()
    {
	this.ctx=null;
    }
    
    public void ejbLoad()
    {}

    public void ejbStore()
    {}

    //    public void ejbRemove()
    //    {}    
}    
