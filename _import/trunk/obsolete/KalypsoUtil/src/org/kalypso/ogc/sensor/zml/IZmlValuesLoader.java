package org.kalypso.ogc.sensor.zml;

import java.text.ParseException;
import java.util.List;


/**
 * @author schlienger
 *
 */
public interface IZmlValuesLoader
{
  public List load( ZmlAxis axis ) throws ParseException;
}
