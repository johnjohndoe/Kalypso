package de.tuhh.wb.javagis.model;

import java.lang.*;
import javax.ejb.CreateException;
import javax.ejb.EntityBean;
import javax.ejb.EntityContext;
import javax.ejb.FinderException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;

import de.tuhh.wb.javagis.property.VectorSet;
import de.tuhh.wb.javagis.model.VersionLocal;

public interface RelationLocal extends ElementLocal
{
    //    public Integer ejbCreate(Integer objectId, VersionLocal version) throws CreateException
    public Object getSrcId();
    public Object getDestId();

    public String getSrcKey();
    public String getDestKey();
}
