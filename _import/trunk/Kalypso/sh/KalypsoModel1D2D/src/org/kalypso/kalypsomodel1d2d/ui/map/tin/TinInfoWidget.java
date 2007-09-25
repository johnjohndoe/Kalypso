/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraﬂe 22
 *  21073 Hamburg, Germany
 *  http://www.tuhh.de/wb
 *
 *  and
 *
 *  Bjoernsen Consulting Engineers (BCE)
 *  Maria Trost 3
 *  56070 Koblenz, Germany
 *  http://www.bjoernsen.de
 *
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 *
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 *  Contact:
 *
 *  E-Mail:
 *  belger@bjoernsen.de
 *  schlienger@bjoernsen.de
 *  v.doemming@tuhh.de
 *
 *  ---------------------------------------------------------------------------*/
package org.kalypso.kalypsomodel1d2d.ui.map.tin;

import java.awt.Graphics;
import java.awt.Point;
import java.util.ArrayList;
import java.util.Date;
import java.util.Formatter;
import java.util.List;

import javax.xml.datatype.XMLGregorianCalendar;
import javax.xml.namespace.QName;

import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.ISelectionProvider;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PlatformUI;
import org.kalypso.commons.command.ICommandTarget;
import org.kalypso.commons.xml.NS;
import org.kalypso.contribs.java.util.DateUtilities;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.IValuePropertyType;
import org.kalypso.kalypsomodel1d2d.schema.UrlCatalog1D2D;
import org.kalypso.kalypsomodel1d2d.ui.map.temsys.ApplyElevationWidget;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.map.MapPanel;
import org.kalypso.ogc.gml.map.utilities.MapUtilities;
import org.kalypso.ogc.gml.map.widgets.AbstractWidget;
import org.kalypso.ogc.gml.outline.GisMapOutlineView;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_TriangulatedSurface;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;

/**
 * @author jung
 * 
 */
public class TinInfoWidget extends AbstractWidget
{
  private final ISelectionChangedListener m_selectionListener = new ISelectionChangedListener()
  {
    public void selectionChanged( final SelectionChangedEvent event )
    {
      handleSelectionChanged( event.getSelection() );
    }
  };

  private String m_info = null;

  private Point m_point;

  private ISelectionProvider m_selectionProvider = null;

  private static final QName QNAME_TRIANGULATED_SURFACE = new QName( NS.GML3, "TriangulatedSurface" ); //$NON-NLS-1$

  private final List<TinInfoProvider> m_tins = new ArrayList<TinInfoProvider>();

  public TinInfoWidget( )
  {
    super( Messages.getString( "TinInfoWidget.1" ), Messages.getString( "TinInfoWidget.2" ) ); //$NON-NLS-1$ //$NON-NLS-2$
  }

  public TinInfoWidget( final String name, final String toolTip )
  {
    super( name, toolTip );
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#activate(org.kalypso.commons.command.ICommandTarget,
   *      org.kalypso.ogc.gml.map.MapPanel)
   */
  @Override
  public void activate( final ICommandTarget commandPoster, final MapPanel mapPanel )
  {
    super.activate( commandPoster, mapPanel );

    final IWorkbench workbench = PlatformUI.getWorkbench();
    final IWorkbenchWindow window = workbench.getActiveWorkbenchWindow();
    final IWorkbenchPage page = window.getActivePage();
    final GisMapOutlineView outlineView = (GisMapOutlineView) page.findView( GisMapOutlineView.ID );
    if( outlineView == null )
    {
      System.out.println( Messages.getString( "TinInfoWidget.3" ) ); //$NON-NLS-1$
      return;
    }

    final MapPanel outlineMapPanel = outlineView.getMapPanel();
    if( outlineMapPanel != mapPanel )
    {
      System.out.println( Messages.getString( "TinInfoWidget.4" ) ); //$NON-NLS-1$
      return;
    }

    m_selectionProvider = outlineView.getSite().getSelectionProvider();
    m_selectionProvider.addSelectionChangedListener( m_selectionListener );
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#finish()
   */
  @Override
  public void finish( )
  {
    super.finish();

    if( m_selectionProvider != null )
    {
      m_selectionProvider.removeSelectionChangedListener( m_selectionListener );
      m_selectionProvider = null;
    }
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#moved(java.awt.Point)
   */
  @Override
  public void moved( final Point p )
  {
    final GM_Point location = MapUtilities.transform( getMapPanel(), p );

    // get height from selection and remember it
    final StringBuffer sb = new StringBuffer();
    final Formatter formatter = new Formatter( sb );
    for( final TinInfoProvider info : m_tins )
    {
      // final String label = info.getThemeLabel();
      final String unit = info.getFeatureUnit();
      final Date featureDate = info.getFeatureDate();
      final String date;
      if( featureDate == null )
        date = ""; //$NON-NLS-1$
      else
        date = featureDate.toString();

      final String parameter = info.getFeatureParameter();
      final double elevation = info.getValue( location );

      if( Double.isNaN( elevation ) )
        formatter.format( "%17s:  -%n", parameter ); //$NON-NLS-1$
      else
        formatter.format( "%17s: %.2f %4s (%s)%n", parameter, elevation, unit, date ); //$NON-NLS-1$
    }

    // repaint map if tooltip has changed
    m_info = sb.toString().trim();
    if( m_info.length() == 0 )
      m_info = Messages.getString( "TinInfoWidget.8" ); //$NON-NLS-1$

    m_point = p;

    getMapPanel().repaint();
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#paint(java.awt.Graphics)
   */
  @Override
  public void paint( final Graphics g )
  {
    ApplyElevationWidget.drawToolTip( m_info, m_point, g );
  }

  protected void handleSelectionChanged( final ISelection selection )
  {
    m_tins.clear();

    final IStructuredSelection sel = (IStructuredSelection) selection;
    final Object[] selectedElements = sel.toArray();
    for( final Object object : selectedElements )
    {
      if( object instanceof IKalypsoFeatureTheme )
      {
        final IKalypsoFeatureTheme theme = (IKalypsoFeatureTheme) object;
        final IFeatureType featureType = theme.getFeatureType();
        if( featureType == null )
          break;
        final IValuePropertyType[] geomProperties = featureType.getAllGeomteryProperties();
        for( final IValuePropertyType geomPT : geomProperties )
        {
          final QName valueQName = geomPT.getValueQName();
          if( QNAME_TRIANGULATED_SURFACE.equals( valueQName ) )
          {
            // HACK: at the moment these qnames are hard-coded, where to get them from?
            final QName unitQName = new QName( UrlCatalog1D2D.MODEL_1D2DResults_NS, "unit" ); //$NON-NLS-1$
            final QName paramQName = new QName( UrlCatalog1D2D.MODEL_1D2DResults_NS, "parameter" ); //$NON-NLS-1$
            final QName dateQName = new QName( UrlCatalog1D2D.MODEL_1D2DResults_NS, "date" ); //$NON-NLS-1$

            final TinInfoProvider tinInfoProvider = new TinInfoProvider( theme, geomPT, unitQName, paramQName, dateQName );
            m_tins.add( tinInfoProvider );
          }
        }
      }
    }
  }

  private final static class TinInfoProvider
  {
    private final IKalypsoFeatureTheme m_theme;

    private final IValuePropertyType m_geomPT;

    private final QName m_parameterQname;

    private final QName m_unitQname;

    private final QName m_dateQname;

    public TinInfoProvider( final IKalypsoFeatureTheme theme, final IValuePropertyType geomPT, final QName unitQname, final QName parameterQname, final QName dateQname )
    {
      m_theme = theme;
      m_geomPT = geomPT;
      m_unitQname = unitQname;
      m_dateQname = dateQname;
      m_parameterQname = parameterQname;
    }

    public String getThemeLabel( )
    {
      return m_theme.getName();
    }

    public Date getFeatureDate( )
    {
      // find first theme which covers the position
      final FeatureList featureList = m_theme.getFeatureList();
      if( featureList == null || featureList.size() == 0 )
        return null;

      final GMLWorkspace workspace = m_theme.getWorkspace();
      for( final Object object : featureList )
      {
        final Feature feature = FeatureHelper.getFeature( workspace, object );
        if( feature == null )
          continue;

        return DateUtilities.toDate( (XMLGregorianCalendar) feature.getProperty( m_dateQname ) );
      }

      return null;
    }

    public final String getFeatureUnit( )
    {
      // find first theme which covers the position
      final FeatureList featureList = m_theme.getFeatureList();
      if( featureList == null || featureList.size() == 0 )
        return ""; //$NON-NLS-1$

      final GMLWorkspace workspace = m_theme.getWorkspace();
      for( final Object object : featureList )
      {
        final Feature feature = FeatureHelper.getFeature( workspace, object );
        if( feature == null )
          continue;

        return (String) feature.getProperty( m_unitQname );
      }

      return ""; //$NON-NLS-1$
    }

    public final String getFeatureParameter( )
    {
      // find first theme which covers the position
      final FeatureList featureList = m_theme.getFeatureList();
      if( featureList == null || featureList.size() == 0 )
        return ""; //$NON-NLS-1$

      final GMLWorkspace workspace = m_theme.getWorkspace();
      for( final Object object : featureList )
      {
        final Feature feature = FeatureHelper.getFeature( workspace, object );
        if( feature == null )
          continue;

        return (String) feature.getProperty( m_parameterQname );
      }

      return ""; //$NON-NLS-1$
    }

    public final double getValue( final GM_Point location )
    {
      // find first theme wich covers the position
      final FeatureList featureList = m_theme.getFeatureList();
      if( featureList == null || featureList.size() == 0 )
        return Double.NaN;

      final GMLWorkspace workspace = m_theme.getWorkspace();
      for( final Object object : featureList )
      {
        final Feature feature = FeatureHelper.getFeature( workspace, object );
        if( feature == null )
          continue;

        final GM_TriangulatedSurface surface = (GM_TriangulatedSurface) feature.getProperty( m_geomPT );
        return surface.getValue( location );
      }

      return Double.NaN;
    }
  }

}
