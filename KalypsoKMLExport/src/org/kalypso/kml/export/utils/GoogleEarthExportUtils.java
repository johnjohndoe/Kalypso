/**
 *
 */
package org.kalypso.kml.export.utils;

import org.kalypsodeegree.filterencoding.FilterEvaluationException;
import org.kalypsodeegree.graphics.sld.LineSymbolizer;
import org.kalypsodeegree.graphics.sld.PointSymbolizer;
import org.kalypsodeegree.graphics.sld.PolygonSymbolizer;
import org.kalypsodeegree.graphics.sld.Symbolizer;
import org.kalypsodeegree.graphics.sld.TextSymbolizer;
import org.kalypsodeegree.model.feature.Feature;

import com.google.earth.kml.FolderType;
import com.google.earth.kml.ObjectFactory;
import com.google.earth.kml.StyleType;

/**
 * @author kuch
 */
public class GoogleEarthExportUtils
{

  /**
   * @param feature
   * @param m_factory
   * @param symbolizer
   * @return
   * @throws FilterEvaluationException
   */
  public static StyleType getStyleType( final ObjectFactory factory, final Feature feature, final Symbolizer symbolizer ) throws FilterEvaluationException
  {
    final StyleTypeFactory styleFactory = StyleTypeFactory.getStyleFactory( factory );

    if( symbolizer instanceof PointSymbolizer )
      return styleFactory.getPointSymbolizer( (PointSymbolizer) symbolizer );
    else if( symbolizer instanceof LineSymbolizer )
      return styleFactory.getLineSymbolizer( (LineSymbolizer) symbolizer );
    else if( symbolizer instanceof PolygonSymbolizer )
      return styleFactory.getPolygonSymbolizer( (PolygonSymbolizer) symbolizer );
    else if( symbolizer instanceof TextSymbolizer )
      return null;

    return null;
    // FIXME
// throw (new NotImplementedException());
  }

  /**
   * @param folderType
   */
  public static void removeEmtpyFolders( final FolderType folderType )
  {
    // TODO:
  }

}
