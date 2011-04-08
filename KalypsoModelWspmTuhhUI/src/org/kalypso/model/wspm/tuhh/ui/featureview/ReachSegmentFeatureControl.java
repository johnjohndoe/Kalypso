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
package org.kalypso.model.wspm.tuhh.ui.featureview;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.MenuManager;
import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.CheckStateChangedEvent;
import org.eclipse.jface.viewers.CheckboxTableViewer;
import org.eclipse.jface.viewers.ICheckStateListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.StructuredViewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Table;
import org.kalypso.commons.command.ICommand;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.model.wspm.core.gml.IProfileFeature;
import org.kalypso.model.wspm.core.gml.WspmWaterBody;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhReach;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhReachProfileSegment;
import org.kalypso.model.wspm.tuhh.ui.i18n.Messages;
import org.kalypso.ogc.gml.featureview.control.AbstractFeatureControl;
import org.kalypso.ogc.gml.featureview.control.IFeatureControl;
import org.kalypso.ui.editor.gmleditor.ui.GMLLabelProvider;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;

/**
 * @author belger
 * @author jung
 */
public class ReachSegmentFeatureControl extends AbstractFeatureControl implements IFeatureControl
{
  protected final class ChangeCheckstateAction extends Action
  {
    private final boolean m_checkState;

    public ChangeCheckstateAction( final String text, final boolean checkState )
    {
      super( text );
      m_checkState = checkState;
    }

    /**
     * @see org.eclipse.jface.action.Action#run()
     */
    @Override
    public void run( )
    {
      final IStructuredSelection selection = (IStructuredSelection) getViewer().getSelection();
      changeCheckState( selection.toArray(), m_checkState );
    }
  }

  private final Set<ModifyListener> m_listeners = new HashSet<ModifyListener>();

  private CheckboxTableViewer m_viewer;

  public ReachSegmentFeatureControl( final Feature feature, final IPropertyType pt )
  {
    super( feature, pt );
  }

  public StructuredViewer getViewer( )
  {
    return m_viewer;
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.control.IFeatureControl#addModifyListener(org.eclipse.swt.events.ModifyListener)
   */
  @Override
  public void addModifyListener( final ModifyListener l )
  {
    m_listeners.add( l );
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.control.IFeatureControl#removeModifyListener(org.eclipse.swt.events.ModifyListener)
   */
  @Override
  public void removeModifyListener( final ModifyListener l )
  {
    m_listeners.remove( l );
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.control.IFeatureControl#createControl(org.eclipse.swt.widgets.Composite, int)
   */
  @Override
  public Control createControl( final Composite parent, final int style )
  {
    m_viewer = CheckboxTableViewer.newCheckList( parent, style | SWT.CHECK | SWT.MULTI );
    m_viewer.setContentProvider( new ArrayContentProvider() );
    m_viewer.setLabelProvider( new GMLLabelProvider() );

    m_viewer.addCheckStateListener( new ICheckStateListener()
    {
      @Override
      public void checkStateChanged( final CheckStateChangedEvent event )
      {
        final IProfileFeature profileFeature = (IProfileFeature) event.getElement();
        final boolean checked = event.getChecked();
        handleCheckStateChanged( new IProfileFeature[] { profileFeature }, checked );
      }
    } );

    final MenuManager manager = new MenuManager();
    manager.add( new ChangeCheckstateAction( Messages.getString( "org.kalypso.model.wspm.tuhh.ui.featureview.ReachSegmentFeatureControl.0" ), true ) ); //$NON-NLS-1$
    manager.add( new ChangeCheckstateAction( Messages.getString( "org.kalypso.model.wspm.tuhh.ui.featureview.ReachSegmentFeatureControl.1" ), false ) ); //$NON-NLS-1$

    final Table table = m_viewer.getTable();
    table.setMenu( manager.createContextMenu( table ) );

    updateControl();

    return table;
  }

  protected void changeCheckState( final Object[] objects, final boolean check )
  {
    final Collection<IProfileFeature> toToggle = new ArrayList<IProfileFeature>();

    for( final Object object : objects )
    {
      final boolean checked = m_viewer.getChecked( object );
      if( checked != check )
        toToggle.add( (IProfileFeature) object );
    }

    final IProfileFeature[] profilesToCheck = toToggle.toArray( new IProfileFeature[toToggle.size()] );
    if( profilesToCheck.length > 0 )
      handleCheckStateChanged( profilesToCheck, check );
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.control.IFeatureControl#isValid()
   */
  @Override
  public boolean isValid( )
  {
    return true;
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.control.IFeatureControl#updateControl()
   */
  @Override
  public void updateControl( )
  {
    final Feature feature = getFeature();
    if( !(feature instanceof TuhhReach) )
    {
      m_viewer.setInput( null );
      m_viewer.setCheckStateProvider( null );
    }
    else
    {
      final TuhhReach reach = (TuhhReach) feature;
      m_viewer.setCheckStateProvider( new ReachSegmentCheckStateProvider( reach ) );

      final WspmWaterBody waterBody = reach.getWaterBody();
      if( waterBody == null )
        m_viewer.setInput( new Object[] {} );
      else
      {
        final List< ? > profiles = (List< ? >) waterBody.getProperty( WspmWaterBody.QNAME_PROP_PROFILEMEMBER );
        m_viewer.setInput( profiles );
      }
    }
  }

  protected void handleCheckStateChanged( final IProfileFeature[] profileFeatures, final boolean checked )
  {
    // add or remove profile segment according to new check-state
    final TuhhReach reach = (TuhhReach) getFeature();
    final ICommand changeCommand = createCommand( reach, profileFeatures, checked );
    fireFeatureChange( changeCommand );
  }

  private ICommand createCommand( final TuhhReach reach, final IProfileFeature[] profileFeatures, final boolean checked )
  {
    if( checked )
      return new AddReachSegementCommand( reach, profileFeatures );
    else
    {
      final TuhhReachProfileSegment[] segmentsToRemove = findSegments( reach, profileFeatures );
      return new RemoveReachSegmentCommand( reach, segmentsToRemove );
    }
  }

  private TuhhReachProfileSegment[] findSegments( final TuhhReach reach, final IProfileFeature[] profileFeatures )
  {
    final Collection<TuhhReachProfileSegment> segments = new ArrayList<TuhhReachProfileSegment>( profileFeatures.length );

    final FeatureList segmentList = (FeatureList) reach.getProperty( TuhhReach.QNAME_PROP_REACHSEGMENTMEMBER );

    for( final IProfileFeature profileFeature : profileFeatures )
    {
      final TuhhReachProfileSegment segment = findSegment( segmentList, profileFeature );
      if( segment != null )
        segments.add( segment );
    }

    return segments.toArray( new TuhhReachProfileSegment[segments.size()] );
  }

  private TuhhReachProfileSegment findSegment( final FeatureList segmentList, final IProfileFeature profileFeature )
  {
    // CAUTION: this is a linear search through a list, and so a potential performance problem
    for( final Object segmentFeature : segmentList )
    {
      final TuhhReachProfileSegment segment = (TuhhReachProfileSegment) segmentFeature;
      final IProfileFeature profileMember = segment.getProfileMember();
      if( profileMember != null && profileMember.equals( profileFeature ) )
        return segment;
    }

    return null;
  }
}
