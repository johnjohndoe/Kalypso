package org.kalypso.ogc.sensor;



/**
 * Ein Achsen beinhaltet Werte und wird mit eine Einheit und eine Beschriftung beschrieben.
 * 
 * @author schlienger
 */
public interface IAxis
{
  /** returns the class of the data in this axis */
  public Class getDataClass();
  
  /** returns the application dependent type of this axis */
  public String getType();
  
  /** returns the unit of this axis */
  public String getUnit();
  
  /** returns the label of this axis */
  public String getLabel();
  
  /** returns the position of this axis in the tupple */
  public int getPosition();
}
