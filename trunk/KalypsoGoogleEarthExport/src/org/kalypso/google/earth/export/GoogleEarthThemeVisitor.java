/**
 *
 */
package org.kalypso.google.earth.export;

import java.util.List;

import javax.xml.bind.JAXBElement;

import org.eclipse.core.runtime.CoreException;
import org.kalypso.google.earth.export.utils.ThemeGoogleEarthExportable;
import org.kalypso.ogc.gml.AbstractCascadingLayerTheme;
import org.kalypso.ogc.gml.GisTemplateMapModell;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.map.MapPanel;
import org.kalypso.ogc.gml.mapmodel.IKalypsoThemePredicate;
import org.kalypso.ogc.gml.mapmodel.visitor.KalypsoThemeVisitor;

import com.google.earth.kml._2.FeatureType;
import com.google.earth.kml._2.FolderType;
import com.google.earth.kml._2.ObjectFactory;

/**
 * @author kuch
 */
public class GoogleEarthThemeVisitor extends KalypsoThemeVisitor
{

  private final FolderType m_parent;

  private final MapPanel mapPanel;

  /**
   * @param containers
   * @param predicate
   */
  public GoogleEarthThemeVisitor( final MapPanel mapPanel, final FolderType parent, final IKalypsoThemePredicate predicate )
  {
    super( predicate );
    this.mapPanel = mapPanel;
    m_parent = parent;
  }

  /*
   * (non-Javadoc)
   * 
   * @see org.kalypso.ogc.gml.mapmodel.visitor.KalypsoThemeVisitor#visit(org.kalypso.ogc.gml.IKalypsoTheme)
   */
  @Override
  public boolean visit( final IKalypsoTheme theme )
  {

    final ObjectFactory factory = new ObjectFactory();
    final List<JAXBElement< ? extends FeatureType>> myList = m_parent.getFeature();

    if( theme instanceof AbstractCascadingLayerTheme )
    {
      final FolderType folderType = factory.createFolderType();

      folderType.setName( theme.getName() );

      final AbstractCascadingLayerTheme cascading = (AbstractCascadingLayerTheme) theme;
      final GisTemplateMapModell inner = cascading.getInnerMapModel();
      final GoogleEarthThemeVisitor visitor = new GoogleEarthThemeVisitor( mapPanel, folderType, new ThemeGoogleEarthExportable() );

      final IKalypsoTheme[] themes = inner.getAllThemes();

      for( final IKalypsoTheme innerTheme : themes )
        visitor.visit( innerTheme );

      myList.add( factory.createFolder( folderType ) );
    }
    else if( theme instanceof IKalypsoFeatureTheme )
      try
      {
        final FolderType folderType = factory.createFolderType();
        folderType.setName( theme.getName() );
        final IKalypsoFeatureTheme ft = (IKalypsoFeatureTheme) theme;
        final GoogleExportDelegate delegate = new GoogleExportDelegate( mapPanel, factory, folderType );

        ft.paintInternal( delegate );

        myList.add( factory.createFolder( folderType ) );
      }
      catch( final CoreException e )
      {
        e.printStackTrace();
      }

    return false;
  }
}
