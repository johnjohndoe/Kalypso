package de.tuhh.wb.javagis.model;

import java.lang.*;
import javax.ejb.CreateException;
import javax.ejb.EntityBean;
import javax.ejb.EntityContext;
import javax.ejb.FinderException;
import java.util.Vector;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import de.tuhh.wb.javagis.property.VectorSet;
import de.tuhh.wb.javagis.model.VersionLocal;


public interface ObjectLocal extends ElementLocal
{
    //public Integer create(Integer objectId, VersionLocal version) throws CreateException;
    public Vector returnForwardRelations();
    public Vector returnBackwardRelations();
}
