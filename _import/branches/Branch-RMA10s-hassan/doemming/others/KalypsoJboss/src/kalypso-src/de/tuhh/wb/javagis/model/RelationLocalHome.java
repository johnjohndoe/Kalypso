package de.tuhh.wb.javagis.model;

import java.lang.*;
import javax.ejb.CreateException;
import javax.ejb.EntityBean;
import javax.ejb.EntityContext;
import javax.ejb.FinderException;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;

import de.tuhh.wb.javagis.property.VectorSet;
import de.tuhh.wb.javagis.model.VersionLocal;
import de.tuhh.wb.javagis.model.Tools;

public interface RelationLocalHome extends ElementLocalHome
{
    //   public de.tuhh.wb.javagis.model.na.ChannelLocal create(java.lang.Integer objectId,de.tuhh.wb.javagis.model.VersionLocal version) throws javax.ejb.CreateException;
    // its not possible to use "create" as interface-method, because of the returntype :-(
}
