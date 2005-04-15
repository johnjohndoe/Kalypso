package org.kalypso.ogc.gml.convert;

import java.net.URL;

import org.kalypso.gml.util.CsvSourceType;
import org.kalypso.gml.util.CsvTargetType;
import org.kalypso.gml.util.FeaturemappingSourceType;
import org.kalypso.gml.util.GmlSourceType;
import org.kalypso.gml.util.Gmltarget;
import org.kalypso.gml.util.SourceType;
import org.kalypso.gml.util.TargetType;
import org.kalypso.java.net.IUrlResolver;
import org.kalypso.ogc.gml.convert.source.CsvSourceHandler;
import org.kalypso.ogc.gml.convert.source.FeaturemappingSourceHandler;
import org.kalypso.ogc.gml.convert.source.GmlSourceHandler;
import org.kalypso.ogc.gml.convert.source.ISourceHandler;
import org.kalypso.ogc.gml.convert.target.GmlTargetHandler;
import org.kalypso.ogc.gml.convert.target.ITargetHandler;
import org.kalypsodeegree.model.feature.GMLWorkspace;

/**
 * Dreh- und Angelpunkt des GML-Konvertierens.
 * 
 * @author belger
 */
public class GmlConvertFactory
{
  private GmlConvertFactory()
  {
  // wird nicht instantiiert
  }

  /**
   * Lädt das GML aus einer Source. Sorgt intern dafür, dass die richtigen
   * Source-Handler benutzt werden.
   * 
   * @throws GmlConvertException
   */
  public static final GMLWorkspace loadSource( final IUrlResolver resolver, final URL context, final SourceType source )
      throws GmlConvertException
  {
    // switch over source-type
    final ISourceHandler handler;
    if( source instanceof FeaturemappingSourceType )
      handler = new FeaturemappingSourceHandler( resolver, context, (FeaturemappingSourceType)source );
    else if( source instanceof CsvSourceType )
      handler = new CsvSourceHandler( resolver, context, (CsvSourceType)source );
    else if( source instanceof GmlSourceType )
      handler = new GmlSourceHandler( resolver, context, (GmlSourceType)source );
    else
      throw new GmlConvertException( "Unbekannter Source-Type: " + source.getClass().getName() );

    return handler.getWorkspace();
  }

  /**
   * Schreibt das GML ins angegebene Target.
   * @throws GmlConvertException
   *  
   */
  public static void writeIntoTarget( final IUrlResolver resolver, final URL context,
      final GMLWorkspace gml, final TargetType target ) throws GmlConvertException
  {
    final ITargetHandler handler;
    if( target instanceof CsvTargetType )
      handler = new CsvTargetHandler( resolver, context, (CsvTargetType)target );
    else if( target instanceof Gmltarget )
      handler = new GmlTargetHandler( resolver, context, (Gmltarget)target );
    else
      throw new GmlConvertException( "Unbekannter Target-Type: " + target.getClass().getName() );

    handler.saveWorkspace( gml );
  }
}
