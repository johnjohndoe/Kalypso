package de.tuhh.wb.javagis.model;

import java.util.Vector;


public interface ObjectLocal extends ElementLocal
{
    //public Integer create(Integer objectId, VersionLocal version) throws CreateException;
    public Vector returnForwardRelations();
    public Vector returnBackwardRelations();
}
