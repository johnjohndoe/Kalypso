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
package org.kalypso.model.wspm.pdb.ui.internal.wspm;

import java.util.HashSet;
import java.util.Set;

import org.apache.commons.lang.StringUtils;
import org.eclipse.core.databinding.DataBindingContext;
import org.eclipse.core.databinding.observable.set.IObservableSet;
import org.eclipse.core.databinding.observable.set.ISetChangeListener;
import org.eclipse.core.databinding.observable.set.SetChangeEvent;
import org.eclipse.jface.databinding.viewers.IViewerObservableSet;
import org.eclipse.jface.databinding.viewers.ViewersObservables;
import org.eclipse.jface.viewers.CheckboxTreeViewer;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Tree;
import org.kalypso.commons.databinding.DataSetBinder;
import org.kalypso.gmlschema.annotation.IAnnotation;
import org.kalypso.model.wspm.core.gml.WspmWaterBody;
import org.kalypso.model.wspm.pdb.db.mapping.WaterBody;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhReachProfileSegment;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhWspmProject;
import org.kalypso.ui.editor.gmleditor.part.GMLContentProvider;
import org.kalypso.ui.editor.gmleditor.part.GMLLabelProvider;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;
import org.kalypsodeegree_impl.model.feature.gmlxpath.GMLXPath;

/**
 * Choose the reaches that should be imported into the csdb.
 * 
 * @author Gernot Belger
 */
public class CheckinStateChooseElementsPage extends WizardPage
{
  private final CheckinStateData m_data;

  private DataBindingContext m_binding;

  private final Set<String> m_waterBodies = new HashSet<String>();

  protected CheckinStateChooseElementsPage( final String pageName, final CheckinStateData data )
  {
    super( pageName );

    m_data = data;
    final WaterBody[] existingWaterBodies = m_data.getExistingWaterBodies();
    for( final WaterBody waterBody : existingWaterBodies )
    {
      final String name = waterBody.getName();
      m_waterBodies.add( name );
    }

    setTitle( "Select Elements" );
    setDescription( "Please select the elements that should be uploaded into the cross section database." );
  }

  @Override
  public void createControl( final Composite parent )
  {
    m_binding = new DataBindingContext();

    final CheckboxTreeViewer treeViewer = new CheckboxTreeViewer( parent, SWT.BORDER );
    final Tree tree = treeViewer.getTree();
    setControl( tree );

    treeViewer.setAutoExpandLevel( 2 );

    treeViewer.setLabelProvider( new GMLLabelProvider() );
    final GMLContentProvider provider = new GMLContentProvider( false, false );
    /* Hide profiles */
    provider.setShowChildreOverride( WspmWaterBody.QNAME_MEMBER_PROFILE, false );
    provider.setShowChildreOverride( TuhhReachProfileSegment.QNAME_PROFILEREACHSEGMENT, false );
    treeViewer.setContentProvider( provider );
    provider.setRootPath( new GMLXPath( new GMLXPath( TuhhWspmProject.QNAME ), TuhhWspmProject.QNAME_MEMBER_WATER_BODY ) );

    treeViewer.setInput( m_data.getProject() );

    final IViewerObservableSet target = ViewersObservables.observeCheckedElements( treeViewer, Set.class );
    final IObservableSet model = m_data.getCheckedElements();

    final DataSetBinder binder = new DataSetBinder( target, model );
    binder.apply( m_binding );

    // ? Is there another (binding-)way to validate the empty set?
    model.addSetChangeListener( new ISetChangeListener()
    {
      @Override
      public void handleSetChange( final SetChangeEvent event )
      {
        checkstateChanged( model );
      }
    } );

    setPageComplete( !model.isEmpty() );
    checkExistingWaterBodies( model );
  }

  protected void checkstateChanged( final IObservableSet model )
  {
    setMessage( null );
    setPageComplete( true );

    final boolean isEmpty = model.isEmpty();
    if( isEmpty )
    {
      setMessage( "No elements selected", WARNING );
      setPageComplete( false );
      return;
    }

    checkExistingWaterBodies( model );
  }

  private void checkExistingWaterBodies( final IObservableSet model )
  {
    for( final Object checkedElement : model )
    {
      final WspmWaterBody wspmWaterBody = asWaterBody( checkedElement );
      if( wspmWaterBody != null )
      {
        final String label = FeatureHelper.getAnnotationValue( wspmWaterBody, IAnnotation.ANNO_LABEL );
        final String baseMessage = String.format( "Unable to upload water body '%s': ", label );

        final String refNr = wspmWaterBody.getRefNr();
        if( StringUtils.isBlank( refNr ) )
        {
          setMessage( baseMessage + "river code is not set.", WARNING );
          setPageComplete( false );
          return;
        }

        if( !m_waterBodies.contains( refNr ) )
        {
          final String detailMessage = String.format( "river code '%s' does not exist in database.%nChange rivercode or add a new water body to database first.", refNr );
          setMessage( baseMessage + detailMessage, WARNING );
          setPageComplete( false );
          return;
        }
      }
    }
  }

  private WspmWaterBody asWaterBody( final Object element )
  {
    if( element instanceof WspmWaterBody )
      return (WspmWaterBody) element;

    if( element instanceof Feature )
      return asWaterBody( ((Feature) element).getParent() );

    return null;
  }
}