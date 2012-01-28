/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestra�e 22
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
package org.kalypso.model.wspm.tuhh.ui.export.bankline;

import java.net.URL;
import java.nio.charset.Charset;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import org.apache.commons.lang3.StringUtils;
import org.eclipse.core.resources.IProject;
import org.eclipse.jface.dialogs.IDialogSettings;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.kalypso.commons.java.util.AbstractModelObject;
import org.kalypso.contribs.eclipse.core.resources.ResourceUtilities;
import org.kalypso.model.wspm.core.gml.WspmProject;
import org.kalypso.model.wspm.core.gml.WspmReach;
import org.kalypso.model.wspm.core.gml.WspmWaterBody;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhReach;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;
import org.kalypsodeegree.model.feature.IFeatureRelation;

/**
 * @author Gernot Belger
 */
public class BanklineExportData extends AbstractModelObject
{
  private final BanklineMarkerProviderFactory m_banklineMarkerProviderFactory = new BanklineMarkerProviderFactory();

  private static final String UNKNOWN_PROJECT_NAME = "Unknown";

  public static final String PROPERTY_MARKER_CHOOSER = "markerChooser"; //$NON-NLS-1$

  private final Collection<Feature> m_exportableElements = new ArrayList<>();

  private final IBanklineMarkerProvider[] m_availableMarkerChoosers = m_banklineMarkerProviderFactory.getAvailableProviders();

  private WspmProject m_project;

  private String m_exportCrs;

  private Charset m_exportCharset;

  private boolean m_doWritePrj;

  private String m_exportShapeBase;

  private IBanklineMarkerProvider m_markerChooser = m_availableMarkerChoosers[0];

  public void init( final IDialogSettings settings, final IStructuredSelection selection )
  {
    if( settings == null )
      return;

    findExportableElements( selection );
    findProject();

    /* marker chooser */
    final String chooserId = settings.get( PROPERTY_MARKER_CHOOSER );
    if( !StringUtils.isBlank( chooserId ) )
      m_markerChooser = m_banklineMarkerProviderFactory.getProvider( chooserId );
  }


  public void storeSettings( final IDialogSettings settings )
  {
    if( settings == null )
      return;

    settings.put( PROPERTY_MARKER_CHOOSER, m_markerChooser.getId() );
  }

  private void findExportableElements( final IStructuredSelection selection )
  {
    final List< ? > list = selection.toList();
    addExportableElements( list );
  }

  private void addExportableElements( final List< ? > list )
  {
    for( final Object element : list )
      addExportableElement( element );
  }

  private void addExportableElement( final Object element )
  {
    if( element instanceof TuhhReach )
      m_exportableElements.add( (TuhhReach) element );
    else if( element instanceof WspmWaterBody )
    {
      final WspmWaterBody waterBody = (WspmWaterBody) element;
      m_exportableElements.add( waterBody );
      final IFeatureBindingCollection<WspmReach> reaches = waterBody.getReaches();
      addExportableElements( reaches );
    }
    else if( element instanceof IFeatureRelation )
    {
      final IFeatureRelation relation = (IFeatureRelation) element;
      if( relation.getPropertyType().isList() )
      {
        final List< ? > list = (List< ? >) relation.getValue();
        addExportableElements( list );
      }
    }
  }

  private void findProject( )
  {
    for( final Feature element : m_exportableElements )
    {
      final GMLWorkspace workspace = element.getWorkspace();
      final Feature rootFeature = workspace.getRootFeature();
      if( rootFeature instanceof WspmProject )
      {
        m_project = (WspmProject) rootFeature;
        return;
      }
    }
  }

  String getProjectName( )
  {
    if( m_project == null )
      return UNKNOWN_PROJECT_NAME;

    final URL context = m_project.getWorkspace().getContext();
    if( context == null )
      return UNKNOWN_PROJECT_NAME;

    final IProject project = ResourceUtilities.findProjectFromURL( context );
    if( project == null )
      return UNKNOWN_PROJECT_NAME;

    return project.getName();
  }

  public void setExportCrs( final String exportCrs )
  {
    m_exportCrs = exportCrs;
  }

  public String getExportCrs( )
  {
    return m_exportCrs;
  }

  public void setExportCharset( final Charset exportCharset )
  {
    m_exportCharset = exportCharset;
  }

  public Charset getExportCharset( )
  {
    return m_exportCharset;
  }

  public Feature[] getExportableElements( )
  {
    return m_exportableElements.toArray( new Feature[m_exportableElements.size()] );
  }

  public void setWritePrj( final boolean doWritePrj )
  {
    m_doWritePrj = doWritePrj;
  }

  public boolean getWritePrj( )
  {
    return m_doWritePrj;
  }

  public void setExportShapeBase( final String shapeFilebase )
  {
    m_exportShapeBase = shapeFilebase;
  }

  public String getExportShapeBase( )
  {
    return m_exportShapeBase;
  }

  public IBanklineMarkerProvider[] getAvailableMarkerChooser( )
  {
    return m_availableMarkerChoosers;
  }

  public IBanklineMarkerProvider getMarkerChooser( )
  {
    return m_markerChooser;
  }

  public void setMarkerChooser( final IBanklineMarkerProvider markerChooser )
  {
    final IBanklineMarkerProvider oldValue = m_markerChooser;

    m_markerChooser = markerChooser;

    firePropertyChange( PROPERTY_MARKER_CHOOSER, oldValue, markerChooser );
  }
}