package org.kalypso.ogc.sensor;

import java.util.List;


/**
 * Ein Achsen beinhaltet Werte und wird mit eine Einheit und eine Beschriftung beschrieben.
 * 
 * @author schlienger
 */
public interface IAxis
{
  /** returns the class of the data in this axis */
  public Class getDataClass();
  
  /** returns the unit of this axis */
  public String getUnit();
  
  /** returns the label of this axis */
  public String getLabel();
  
  /** returns the position of this axis in the tupple */
  public int getPosition();
  
  /** returns true when this axis has a restriction on the values that can be assigned */
  public boolean isRestricted();
  
  /** returns the list of the values that can be assigned to items on this axis */
  public List getRestrictedValues();
}
