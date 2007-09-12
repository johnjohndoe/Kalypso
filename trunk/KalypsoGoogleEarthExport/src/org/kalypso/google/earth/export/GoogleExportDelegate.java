/**
 *
 */
package org.kalypso.google.earth.export;

import java.util.List;

import javax.xml.bind.JAXBElement;

import org.kalypso.google.earth.export.convert.ConvertFacade;
import org.kalypso.google.earth.export.convert.IFeatureGeometryFilter;
import org.kalypso.google.earth.export.utils.GoogleEarthExportUtils;
import org.kalypso.ogc.gml.IPaintInternalDelegate;
import org.kalypso.ogc.gml.map.MapPanel;
import org.kalypsodeegree.graphics.displayelements.DisplayElement;
import org.kalypsodeegree.graphics.displayelements.GeometryDisplayElement;
import org.kalypsodeegree.graphics.sld.Symbolizer;
import org.kalypsodeegree.graphics.transformation.GeoTransform;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_Object;

import sun.reflect.generics.reflectiveObjects.NotImplementedException;

import com.google.earth.kml._2.FeatureType;
import com.google.earth.kml._2.FolderType;
import com.google.earth.kml._2.ObjectFactory;
import com.google.earth.kml._2.PlacemarkType;
import com.google.earth.kml._2.StyleType;

/**
 * @author kuch
 */
public class GoogleExportDelegate implements IPaintInternalDelegate
{
  private final FolderType m_folderType;

  private final MapPanel m_mapPanel;

  private final ObjectFactory m_factory;

  /**
   * @param factory
   * @param folderType
   */
  public GoogleExportDelegate( final MapPanel mapPanel, final ObjectFactory factory, final FolderType folderType )
  {
    m_mapPanel = mapPanel;
    m_factory = factory;
    m_folderType = folderType;
  }

  /*
   * (non-Javadoc)
   * 
   * @see org.kalypso.ogc.gml.IPaintInternalDelegate#getBoundingBox()
   */
  public GM_Envelope getBoundingBox( )
  {
    return m_mapPanel.getBoundingBox();
  }

  /*
   * (non-Javadoc)
   * 
   * @see org.kalypso.ogc.gml.IPaintInternalDelegate#getProjection()
   */
  public GeoTransform getProjection( )
  {
    return m_mapPanel.getProjection();
  }

  /*
   * (non-Javadoc)
   * 
   * @see org.kalypso.ogc.gml.IPaintInternalDelegate#getScale()
   */
  public double getScale( )
  {
    return m_mapPanel.getCurrentScale();
  }

  /*
   * (non-Javadoc)
   * 
   * @see org.kalypso.ogc.gml.IPaintInternalDelegate#getSelected()
   */
  public boolean getSelected( )
  {
    return false;
  }

  /*
   * (non-Javadoc)
   * 
   * @see org.kalypso.ogc.gml.IPaintDelegate#paint(org.kalypsodeegree.graphics.displayelements.DisplayElement)
   */
  public void paint( final DisplayElement displayElement )
  {
    try
    {
      final StyleType styleType;

      if( displayElement instanceof GeometryDisplayElement )
      {
        final GeometryDisplayElement element = (GeometryDisplayElement) displayElement;
        final Symbolizer symbolizer = element.getSymbolizer();

        styleType = GoogleEarthExportUtils.getStyleType( m_factory, displayElement.getFeature(), symbolizer );
      }
      else
        throw (new NotImplementedException());

      final IFeatureGeometryFilter filter = new IFeatureGeometryFilter()
      {
        public GM_Object[] getGeometries( Feature f )
        {
          GM_Object geometryProperty = f.getDefaultGeometryProperty();
          return new GM_Object[] { geometryProperty };
        }
      };

      final Feature feature = displayElement.getFeature();
      final PlacemarkType[] placemarks = ConvertFacade.convert( m_factory, feature, filter, styleType );

      final List<JAXBElement< ? extends FeatureType>> features = m_folderType.getFeature();
      for( final PlacemarkType placemark : placemarks )
        features.add( m_factory.createPlacemark( placemark ) );
    }
    catch( final Exception e )
    {
      e.printStackTrace();
    }
  }
}
