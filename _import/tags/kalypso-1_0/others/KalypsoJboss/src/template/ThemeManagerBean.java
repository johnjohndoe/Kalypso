package de.tuhh.wb.javagis.model;
/**
 * generated java-code for BEAN:
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
 * @ejb:bean   name="ThemeManager"
 *             jndi-name="ejb/ThemeManager"
 *             local-jndi-name="ejb/ThemeManager"
 *             view-type="remote"
 *             type="CMP"
 *             cmp-version="2.x"
 *             primkey-field="key"
 * @ejb:pk generate="false" class="java.lang.String"
 */

public abstract class ThemeManagerBean implements EntityBean
{
    private transient EntityContext ctx;
    
    /**
     * @ejb:interface-methode
     * @ejb:persistent-field
     */      
    public abstract void setKey(java.lang.String key);
    
    /**
     * @ejb:interface-methode
     * @ejb:persistent-field
     */
    public abstract java.lang.String getKey();
    
    /**
     * @ejb:interface-methode
     * @ejb:persistent-field
     */
    public abstract void setName(String name);
    
    /**
     * @ejb:interface-methode
     * @ejb:persistent-field
     */
    public abstract String getName();

    /**
     * @ejb:interface-methode
     * @ejb:persistent-field
     */
    public abstract void setDescription(String description);
    
    /**
     * @ejb:interface-methode
     * @ejb:persistent-field
     */
    public abstract String getDescription();

    /**
     * @ejb:create-method
     */
    public String ejbCreate(String key,String name,String description) throws CreateException
    {
	if(key == null)  throw new CreateException("key is null :-(");
	this.setKey(key);
	this.setName(name);
	this.setDescription(description);
	return null;
    }
    
    public void ejbPostCreate(String key,String name,String description)
    {
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

}    
    


    
