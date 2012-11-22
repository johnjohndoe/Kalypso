/** This file is part of Kalypso
 *
 *  Copyright (c) 2012 by
 *
 *  Björnsen Beratende Ingenieure GmbH, Koblenz, Germany (Bjoernsen Consulting Engineers), http://www.bjoernsen.de
 *  Technische Universität Hamburg-Harburg, Institut für Wasserbau, Hamburg, Germany
 *  (Technical University Hamburg-Harburg, Institute of River and Coastal Engineering), http://www.tu-harburg.de/wb/
 *
 *  Kalypso is free software: you can redistribute it and/or modify it under the terms  
 *  of the GNU Lesser General Public License (LGPL) as published by the Free Software 
 *  Foundation, either version 3 of the License, or (at your option) any later version.
 *
 *  Kalypso is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied 
 *  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with Kalypso.  If not, see <http://www.gnu.org/licenses/>.
 */
package org.kalypso.kalypsomodel1d2d.ui.map.dikeditchgen;

import javax.xml.namespace.QName;

import org.eclipse.jface.layout.GridLayoutFactory;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.kalypso.contribs.eclipse.jface.wizard.IUpdateable;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.gmlschema.property.IPropertyTypeFilter;
import org.kalypso.gmlschema.property.IValuePropertyType;
import org.kalypso.gmlschema.property.PropertyUtils;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.IKalypsoThemeFilter;
import org.kalypso.ogc.gml.mapmodel.IMapModell;
import org.kalypso.ui.addlayer.ThemeAndPropertyChooserGroup;
import org.kalypso.ui.addlayer.ThemeAndPropertyChooserGroup.PropertyDescriptor;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.geometry.GM_AbstractSurface;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree.model.geometry.GM_MultiCurve;
import org.kalypsodeegree.model.geometry.GM_MultiSurface;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.model.geometry.GM_Polygon;

/**
 * @author kurzbach
 */
public class CreateDitchControl extends Composite implements IUpdateable
{
  // parameters
  private final double m_innerWidthFraction = 0.5;

  private final double m_minimumDepth = 0.0;

  private ThemeAndPropertyChooserGroup m_networkThemeChooser;

  private ThemeAndPropertyChooserGroup m_boundaryThemeChooser;

  protected PropertyDescriptor m_curveGeometryPd;

  protected PropertyDescriptor m_polygonGeometryPd;

  private PropertyDescriptor m_startWidthPd;

  private PropertyDescriptor m_endWidthPd;

  public CreateDitchControl( Composite body, FormToolkit toolkit, IMapModell mapModell )
  {
    super( body, SWT.NONE );
    toolkit.adapt( this );
    GridLayoutFactory.swtDefaults().applyTo( this );

    final IKalypsoThemeFilter boundaryThemeFilter = new IKalypsoThemeFilter()
    {

      @Override
      public boolean accept( IKalypsoTheme theme )
      {
        if( theme instanceof IKalypsoFeatureTheme )
        {
          final IKalypsoFeatureTheme featureTheme = (IKalypsoFeatureTheme)theme;
          final IFeatureType featureType = featureTheme.getFeatureType();
          if( featureType != null )
          {
            final IPropertyType[] polygoneProperties = PropertyUtils.filterProperties( featureType, m_polygonGeometryPd.filter );
            if( polygoneProperties.length > 0 )
              return true;
          }
        }

        return false;
      }
    };
    m_boundaryThemeChooser = new ThemeAndPropertyChooserGroup( this, mapModell, boundaryThemeFilter, makeBoundaryPropertyDescriptors() );
    final Group boundaryThemeGroup = m_boundaryThemeChooser.createControl( body );
    boundaryThemeGroup.setText( "Boundary theme" );
    toolkit.adapt( boundaryThemeGroup );
    boundaryThemeGroup.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );

    final IKalypsoThemeFilter networkThemeFilter = new IKalypsoThemeFilter()
    {

      @Override
      public boolean accept( IKalypsoTheme theme )
      {
        if( theme instanceof IKalypsoFeatureTheme )
        {
          final IKalypsoFeatureTheme featureTheme = (IKalypsoFeatureTheme)theme;
          final IFeatureType featureType = featureTheme.getFeatureType();
          if( featureType != null )
          {
            final IPropertyType[] polygoneProperties = PropertyUtils.filterProperties( featureType, m_curveGeometryPd.filter );
            if( polygoneProperties.length > 0 )
              return true;
          }
        }

        return false;
      }
    };

    m_networkThemeChooser = new ThemeAndPropertyChooserGroup( this, mapModell, networkThemeFilter, makeNetworkPropertyDescriptors() );
    final Group networkThemeGroup = m_networkThemeChooser.createControl( body );
    networkThemeGroup.setText( "Network theme" );
    toolkit.adapt( networkThemeGroup );
    networkThemeGroup.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );
  }

  public double getInnerWidthFraction( )
  {
    return m_innerWidthFraction;
  }

  public double getMinimumDepth( )
  {
    return m_minimumDepth;
  }

  private PropertyDescriptor[] makeNetworkPropertyDescriptors( )
  {
    final IPropertyTypeFilter geomFilter = new IPropertyTypeFilter()
    {
      @Override
      public boolean accept( final IPropertyType pt )
      {
        if( !(pt instanceof IValuePropertyType) )
          return false;

        final IValuePropertyType pt2 = (IValuePropertyType)pt;
        if( !pt2.isGeometry() )
          return false;

        final QName valueQName = pt2.getValueQName();
        return valueQName.equals( GM_Curve.CURVE_ELEMENT ) || valueQName.equals( GM_MultiCurve.MULTI_CURVE_ELEMENT );
      }
    };
    m_curveGeometryPd = new PropertyDescriptor( "&Geometry", geomFilter, true );
    final IPropertyTypeFilter doubleFilter = new IPropertyTypeFilter()
    {

      @Override
      public boolean accept( IPropertyType pt )
      {
        if( !(pt instanceof IValuePropertyType) )
          return false;

        final IValuePropertyType pt2 = (IValuePropertyType)pt;
        return Double.class.equals( pt2.getValueClass() );
      }
    };
    m_startWidthPd = new PropertyDescriptor( "Width (&Start)", doubleFilter, true );
    m_endWidthPd = new PropertyDescriptor( "Width (&End)", doubleFilter, true );
    final PropertyDescriptor[] pds = new PropertyDescriptor[] { m_curveGeometryPd, m_startWidthPd, m_endWidthPd };
    return pds;
  }

  private PropertyDescriptor[] makeBoundaryPropertyDescriptors( )
  {
    final IPropertyTypeFilter geomFilter = new IPropertyTypeFilter()
    {
      @Override
      public boolean accept( final IPropertyType pt )
      {
        if( !(pt instanceof IValuePropertyType) )
          return false;

        final IValuePropertyType pt2 = (IValuePropertyType)pt;
        if( !pt2.isGeometry() )
          return false;

        final QName valueQName = pt2.getValueQName();
        return valueQName.equals( GM_AbstractSurface.SURFACE_ELEMENT ) || valueQName.equals( GM_MultiSurface.MULTI_SURFACE_ELEMENT );
      }
    };
    m_polygonGeometryPd = new PropertyDescriptor( "&Geometry", geomFilter, true );
    final PropertyDescriptor[] pds = new PropertyDescriptor[] { m_polygonGeometryPd };
    return pds;
  }

  public IPropertyType getNetworkGeometryProperty( )
  {
    return m_networkThemeChooser.getProperty( m_curveGeometryPd );
  }

  public IPropertyType getNetworkStartWidthProperty( )
  {
    return m_networkThemeChooser.getProperty( m_startWidthPd );
  }

  public IPropertyType getNetworkEndWidthProperty( )
  {
    return m_networkThemeChooser.getProperty( m_endWidthPd );
  }

  @Override
  public void update( )
  {
    // TODO Auto-generated method stub

  }

  public FeatureList getNetworkFeatures( )
  {
    final IKalypsoFeatureTheme theme = (IKalypsoFeatureTheme)m_networkThemeChooser.getTheme();
    if( theme == null )
      return null;

    return theme.getFeatureList();
  }

  public GM_Polygon getBoundaryPolygon( )
  {
    final IKalypsoFeatureTheme theme = (IKalypsoFeatureTheme)m_boundaryThemeChooser.getTheme();
    if( theme == null )
      return null;

    final FeatureList featureList = theme.getFeatureList();
    if( !featureList.isEmpty() )
    {
      final GM_Object geometry = (GM_Object)featureList.getResolved( 0 ).getProperty( m_boundaryThemeChooser.getProperty( m_polygonGeometryPd ) );
      if( geometry instanceof GM_Polygon )
        return (GM_Polygon)geometry;
      else if( geometry instanceof GM_MultiSurface )

        return ((GM_MultiSurface)geometry).getSurfaceAt( 0 );
      else
        // should never happen
        return null;
    }
    else
      return null;
  }
}
