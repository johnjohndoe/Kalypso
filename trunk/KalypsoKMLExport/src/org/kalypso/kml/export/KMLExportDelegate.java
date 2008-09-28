/**
 *
 */
package org.kalypso.kml.export;

import java.util.List;

import javax.xml.bind.JAXBElement;

import org.apache.commons.lang.NotImplementedException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.kalypso.kml.export.convert.ConvertFacade;
import org.kalypso.kml.export.interfaces.IKMLAdapter;
import org.kalypso.kml.export.utils.GoogleEarthExportUtils;
import org.kalypso.ogc.gml.IPaintDelegate;
import org.kalypsodeegree.graphics.displayelements.DisplayElement;
import org.kalypsodeegree.graphics.displayelements.GeometryDisplayElement;
import org.kalypsodeegree.graphics.sld.Symbolizer;
import org.kalypsodeegree.model.feature.Feature;

import com.google.earth.kml.FeatureType;
import com.google.earth.kml.FolderType;
import com.google.earth.kml.GroundOverlayType;
import com.google.earth.kml.ObjectFactory;
import com.google.earth.kml.PlacemarkType;
import com.google.earth.kml.StyleType;

/**
 * @author kuch
 */
public class KMLExportDelegate implements IPaintDelegate
{
  private final FolderType m_folderType;

  private final ObjectFactory m_factory;

  private final IKMLAdapter[] m_provider;

  public KMLExportDelegate( final IKMLAdapter[] provider, final ObjectFactory factory, final FolderType folderType )
  {
    m_provider = provider;
    m_factory = factory;
    m_folderType = folderType;

  }

  /*
   * (non-Javadoc)
   * 
   * @see org.kalypso.ogc.gml.IPaintDelegate#paint(org.kalypsodeegree.graphics.displayelements.DisplayElement)
   */
  public void paint( final DisplayElement displayElement, final IProgressMonitor monitor )
  {

    final StyleType styleType;

    if( displayElement instanceof GeometryDisplayElement )
    {
      final GeometryDisplayElement element = (GeometryDisplayElement) displayElement;
      final Symbolizer symbolizer = element.getSymbolizer();

      try
      {
        styleType = GoogleEarthExportUtils.getStyleType( m_factory, symbolizer );
        if( styleType == null )
          return;

        final Feature feature = displayElement.getFeature();
        for( final IKMLAdapter adapter : m_provider )
        {
          adapter.registerExportedFeature( feature );
        }

        // TODO perhaps, get rendered GM_Point geometry from symbolizer
        final FeatureType[] featureTypes = ConvertFacade.convert( m_provider, m_factory, element.getGeometry(), styleType, feature );

        final List<JAXBElement< ? extends FeatureType>> features = m_folderType.getFeature();
        for( final FeatureType featureType : featureTypes )
        {
          if( featureType instanceof PlacemarkType )
          {
            features.add( m_factory.createPlacemark( (PlacemarkType) featureType ) );
          }
          else if( featureType instanceof GroundOverlayType )
          {
            features.add( m_factory.createGroundOverlay( (GroundOverlayType) featureType ) );
          }
          else
            throw new NotImplementedException();
        }
      }
      catch( final Exception e )
      {
        e.printStackTrace();
      }
    }
  }
}
