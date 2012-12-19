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
import java.util.List;

import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.MenuManager;
import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.CheckStateChangedEvent;
import org.eclipse.jface.viewers.CheckboxTableViewer;
import org.eclipse.jface.viewers.ICheckStateListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.StructuredViewer;
import org.eclipse.swt.SWT;
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
import org.kalypso.ui.editor.gmleditor.part.GMLLabelProvider;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;

/**
 * @author belger
 * @author jung
 */
public class ReachSegmentFeatureControl extends AbstractFeatureControl
{
  protected final class ChangeCheckstateAction extends Action
  {
    private final boolean m_checkState;

    public ChangeCheckstateAction( final String text, final boolean checkState )
    {
      super( text );

      m_checkState = checkState;
    }

    @Override
    public void run( )
    {
      final IStructuredSelection selection = (IStructuredSelection) getViewer().getSelection();
      changeCheckState( selection.toArray(), m_checkState );
    }
  }

  private CheckboxTableViewer m_linkChecklist;

  public ReachSegmentFeatureControl( final Feature feature, final IPropertyType pt )
  {
    super( feature, pt );
  }

  protected StructuredViewer getViewer( )
  {
    return m_linkChecklist;
  }

  /**
   * @return Always <code>true</code>
   */
  @Override
  public boolean isValid( )
  {
    // can never be invalid
    return true;
  }

  @Override
  public Control createControl( final Composite parent, final int style )
  {
    m_linkChecklist = CheckboxTableViewer.newCheckList( parent, style | SWT.MULTI );
    m_linkChecklist.setContentProvider( new ArrayContentProvider() );
    m_linkChecklist.setLabelProvider( new GMLLabelProvider() );

    m_linkChecklist.addCheckStateListener( new ICheckStateListener()
    {
      @Override
      public void checkStateChanged( final CheckStateChangedEvent event )
      {
        final IProfileFeature profileFeature = (IProfileFeature) event.getElement();
        final boolean checked = event.getChecked();
        handleCheckStateChanged( new IProfileFeature[] { profileFeature }, checked );
      }
    } );

    /* Configure context menu */
    final MenuManager manager = new MenuManager();
    manager.add( new ChangeCheckstateAction( Messages.getString( "org.kalypso.model.wspm.tuhh.ui.featureview.ReachSegmentFeatureControl.0" ), true ) ); //$NON-NLS-1$
    manager.add( new ChangeCheckstateAction( Messages.getString( "org.kalypso.model.wspm.tuhh.ui.featureview.ReachSegmentFeatureControl.1" ), false ) ); //$NON-NLS-1$

    final Table table = m_linkChecklist.getTable();
    table.setMenu( manager.createContextMenu( table ) );

    updateControl();

    return table;
  }

  protected void changeCheckState( final Object[] objects, final boolean check )
  {
    final Collection<IProfileFeature> toToggle = new ArrayList<>();

    for( final Object object : objects )
    {
      final boolean checked = m_linkChecklist.getChecked( object );
      if( checked != check )
      {
        toToggle.add( (IProfileFeature) object );
      }
    }

    final IProfileFeature[] profilesToCheck = toToggle.toArray( new IProfileFeature[toToggle.size()] );
    if( profilesToCheck.length > 0 )
    {
      handleCheckStateChanged( profilesToCheck, check );
    }
  }

  @Override
  public void updateControl( )
  {
    final Feature feature = getFeature();
    if( !(feature instanceof TuhhReach) )
    {
      m_linkChecklist.setInput( null );
      m_linkChecklist.setCheckStateProvider( null );
    }
    else
    {
      final TuhhReach reach = (TuhhReach) feature;
      m_linkChecklist.setCheckStateProvider( new ReachSegmentCheckStateProvider( reach ) );

      final WspmWaterBody waterBody = reach.getWaterBody();
      if( waterBody == null )
      {
        m_linkChecklist.setInput( new Object[] {} );
      }
      else
      {
        final List< ? > profiles = (List< ? >) waterBody.getProperty( WspmWaterBody.MEMBER_PROFILE );
        m_linkChecklist.setInput( profiles );
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
    final Collection<TuhhReachProfileSegment> segments = new ArrayList<>( profileFeatures.length );

    final FeatureList segmentList = (FeatureList) reach.getProperty( TuhhReach.QNAME_MEMBER_REACHSEGMENT );

    for( final IProfileFeature profileFeature : profileFeatures )
    {
      final TuhhReachProfileSegment segment = findSegment( segmentList, profileFeature );
      if( segment != null )
      {
        segments.add( segment );
      }
    }

    /* HACK: we use this time here to cleanup-broken reaches as well: remove everything than cannot be here */
    final WspmWaterBody segmentWater = reach.getWaterBody();
    for( final Object segmentElement : segmentList )
    {
      final TuhhReachProfileSegment segment = (TuhhReachProfileSegment) segmentElement;

      /* Remove segments without profile and segments with a profile from the wrong water body */
      /* This might happen, if the water body was duplicated */
      final IProfileFeature profileMember = segment.getProfileMember();
      if( profileMember == null )
      {
        segments.add( segment );
      }
      else
      {
        final WspmWaterBody water = profileMember.getWater();
        if( !segmentWater.equals( water ) )
        {
          segments.add( segment );
        }
      }
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
