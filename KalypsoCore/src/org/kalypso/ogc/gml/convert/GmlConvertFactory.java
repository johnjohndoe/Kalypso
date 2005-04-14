package org.kalypso.ogc.gml.convert;

import java.net.URL;

import org.kalypso.gml.util.CsvSourceType;
import org.kalypso.gml.util.FeaturemappingSourceType;
import org.kalypso.gml.util.GmlSourceType;
import org.kalypso.gml.util.SourceType;
import org.kalypso.loader.ILoader;
import org.kalypso.loader.ILoaderFactory;
import org.kalypso.template.types.KalypsoLinkType;
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
   * @throws SourceHandlerException
   */
  public static final GMLWorkspace loadSource( final URL context, final SourceType source )
      throws SourceHandlerException
  {
    // switch over source-type
    final ISourceHandler handler;
    if( source instanceof FeaturemappingSourceType )
      handler = new FeaturemappingSourceHandler( context, (FeaturemappingSourceType)source );
    else if( source instanceof CsvSourceType )
      handler = new CsvSourceHandler( context, (CsvSourceType)source );
    else if( source instanceof GmlSourceType )
      handler = new GmlSourceHandler( context, (GmlSourceType)source );
    else
      handler = null;

    return handler == null ? null : handler.getWorkspace();
  }

  /**
   * Schreibt das GML ins angegebene Target.
   * @throws SourceHandlerException
   *  
   */
  public static void writeIntoTarget( final ILoaderFactory factory, final URL context,
      final GMLWorkspace gml, final KalypsoLinkType link ) throws SourceHandlerException
  {
    try
    {
      final String linktype = link.getLinktype();
      final ILoader loader = factory.getLoaderInstance( linktype );
      loader.save( link.getHref(), context, null, gml );
    }
    catch( final Exception e )
    {
      throw new SourceHandlerException( "Daten wurden nicht gespeichert: " + e );
    }
  }
}
