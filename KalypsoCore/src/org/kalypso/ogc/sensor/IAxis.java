package org.kalypso.ogc.sensor;



/**
 * Ein Achsen beinhaltet Werte und wird mit eine Einheit und eine Beschriftung beschrieben.
 * 
 * @author schlienger
 */
public interface IAxis
{
  /**
   * @return the class of the data in this axis
   */
  public Class getDataClass();
  
  /** 
   * @return the application dependent type of this axis
   */
  public String getType();
  
  /** 
   * @return the unit of this axis
   */
  public String getUnit();
  
  /**
   * @return the name of this axis
   */
  public String getName();
  
  /** 
   * @return the position of this axis in the tupple
   */
  public int getPosition();
  
  /**
   * Returns true when this axis is part of the key of the TuppleModel. Key can be used 
   * to avoid duplicate entries in the model.
   * 
   * @return boolean
   */
  public boolean isKey();
}
