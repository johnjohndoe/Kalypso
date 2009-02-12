package de.tuhh.wb.javagis.model;


public interface RelationLocal extends ElementLocal
{
    //    public Integer ejbCreate(Integer objectId, VersionLocal version) throws CreateException
    public Object getSrcId();
    public Object getDestId();

    public String getSrcKey();
    public String getDestKey();
}
