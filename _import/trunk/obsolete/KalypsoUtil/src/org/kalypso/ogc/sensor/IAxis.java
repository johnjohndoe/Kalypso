package org.kalypso.ogc.sensor;

import java.util.List;

/**
 * Ein Achsen beinhaltet Werte und wird mit eine Einheit und eine Beschriftung beschrieben.
 * 
 * @author schlienger
 */
public interface IAxis
{
  public String getUnit();
  
  public String getLabel();
  
  public List getValues();
  
  public void setValues( List values );
}
