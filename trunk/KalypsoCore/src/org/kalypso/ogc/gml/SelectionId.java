package org.kalypso.ogc.gml;

/**
 * @author doemming
 */
public class SelectionId
{
  private final String myName;
  private final int myId;
  public SelectionId(String name,int id)
  {
    myName=name;
    myId=id;
  }
  
  public int getId()
  {
    return myId;
  }
  public String toString()
  {
  return myName;  
  }
  
}
