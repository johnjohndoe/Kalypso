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
package org.kalypso.model.wspm.sobek.core.wizard.pages;

import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.ComboViewer;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.kalypso.model.wspm.sobek.core.Messages;
import org.kalypso.model.wspm.sobek.core.interfaces.IBoundaryNodeLastfallCondition;
import org.kalypso.model.wspm.sobek.core.interfaces.ILastfall;
import org.kalypso.model.wspm.sobek.core.interfaces.ISobekModelMember;
import org.kalypso.model.wspm.sobek.core.interfaces.IBoundaryNode.BOUNDARY_TYPE;
import org.kalypso.model.wspm.sobek.core.interfaces.IBoundaryNodeLastfallCondition.BOUNDARY_CONDITION_TYPE;
import org.kalypso.model.wspm.sobek.core.wizard.pages.renderer.ConstantValueComposite;
import org.kalypso.model.wspm.sobek.core.wizard.pages.renderer.TimeSeriesComposite;
import org.kalypso.ogc.sensor.zml.repository.ZmlObservationItem;

/**
 * @author kuch
 */
public class PageEditBoundaryConditionTimeSeries extends WizardPage
{
  protected BOUNDARY_CONDITION_TYPE m_type;

  protected final IBoundaryConditionGeneral m_settings;

  private final ISobekModelMember m_model;

  protected final IBoundaryNodeLastfallCondition m_condition;

  protected TimeSeriesComposite m_ts;

  protected ConstantValueComposite m_constant;

  public IBoundaryConditionGeneral getSettings( )
  {
    return m_settings;
  }

  public ISobekModelMember getModel( )
  {
    return m_model;
  }

  public PageEditBoundaryConditionTimeSeries( final ISobekModelMember model, final IBoundaryNodeLastfallCondition condition, final IBoundaryConditionGeneral settings )
  {
    super( "editBoundaryConditionTimeSeries" ); //$NON-NLS-1$
    m_model = model;
    m_condition = condition;
    m_settings = settings;

    setTitle( Messages.PageEditBoundaryConditionTimeSeries_1 );
    setDescription( Messages.PageEditBoundaryConditionTimeSeries_2 );

    m_type = condition.getLastUsedType();
  }

  /**
   * @see org.eclipse.jface.dialogs.IDialogPage#createControl(org.eclipse.swt.widgets.Composite)
   */
  public void createControl( final Composite parent )
  {
    setPageComplete( false );

    final Object layoutData = parent.getLayoutData();
    if( layoutData instanceof GridData )
    {
      final GridData pLayout = (GridData) layoutData;
      pLayout.widthHint = 900;
      pLayout.heightHint = 600;
      parent.layout();
    }

    final Composite container = new Composite( parent, SWT.NULL );
    container.setLayout( new GridLayout() );
    setControl( container );

    final Group gMapping = new Group( container, SWT.NONE );
    gMapping.setLayout( new GridLayout() );
    gMapping.setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, false ) );
    gMapping.setText( Messages.PageEditBoundaryConditionTimeSeries_13 );

    final BOUNDARY_CONDITION_TYPE[] input;
    final BOUNDARY_TYPE type = getSettings().getBoundaryNodeType();
    if( BOUNDARY_TYPE.eWQ.equals( type ) )
    {
      input = new BOUNDARY_CONDITION_TYPE[] { BOUNDARY_CONDITION_TYPE.eZml };
    }
    else
    {
      input = new BOUNDARY_CONDITION_TYPE[] { BOUNDARY_CONDITION_TYPE.eZml, BOUNDARY_CONDITION_TYPE.eConstant };
    }

    final ComboViewer viewer = new ComboViewer( gMapping, SWT.READ_ONLY | SWT.BORDER );
    viewer.getCombo().setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, false ) );
    viewer.setLabelProvider( new LabelProvider() );
    viewer.setContentProvider( new ArrayContentProvider() );

    viewer.setInput( input );

    final PageEditBoundaryConditionTimeSeries page = this;

    viewer.addSelectionChangedListener( new ISelectionChangedListener()
    {

      public void selectionChanged( final SelectionChangedEvent event )
      {
        final StructuredSelection selection = (StructuredSelection) event.getSelection();
        m_type = (BOUNDARY_CONDITION_TYPE) selection.getFirstElement();

        /* dispose old "widgets" */
        if( m_constant != null )
        {
          if( !m_constant.isDisposed() )
            m_constant.dispose();

          m_constant = null;
        }

        if( m_ts != null )
        {
          if( !m_ts.isDisposed() )
            m_ts.dispose();

          m_ts = null;
        }

        if( BOUNDARY_CONDITION_TYPE.eConstant.equals( m_type ) )
        {
          m_constant = new ConstantValueComposite( page, container, SWT.NULL );
          m_constant.render( m_condition );
        }
        else if( BOUNDARY_CONDITION_TYPE.eZml.equals( m_type ) )
        {
          m_ts = new TimeSeriesComposite( page, container, SWT.NULL );
          m_ts.render();

        }
        else
          throw new IllegalStateException( Messages.PageEditBoundaryConditionTimeSeries_14 + m_type.name() );

        container.layout();
      }
    } );

    if( BOUNDARY_TYPE.eWQ.equals( type ) )
    {
      viewer.setSelection( new StructuredSelection( BOUNDARY_CONDITION_TYPE.eZml ) );
    }
    else
    {
      viewer.setSelection( new StructuredSelection( m_type ) );
    }

  }

  public ILastfall getLastfall( )
  {
    return m_condition.getLastfall();
  }

  public IBoundaryNodeLastfallCondition getCondition( )
  {
    return m_condition;
  }

  public BOUNDARY_CONDITION_TYPE getTypeOfTimeSeries( )
  {
    return m_type;
  }

  public Double getConstValue( )
  {
    return m_constant.getConstValue();
  }

  public Integer getConstValueIntervall( )
  {
    return m_constant.getConstValueIntervall();
  }

  public ZmlObservationItem getZmlObservationItem( )
  {
    return m_ts.getZmlObservationItem();
  }
}
