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

import java.util.ArrayList;
import java.util.List;

import javax.xml.namespace.QName;

import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.kalypso.commons.command.ICommand;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.model.wspm.sobek.core.Messages;
import org.kalypso.model.wspm.sobek.core.interfaces.IBoundaryNodeLastfallCondition;
import org.kalypso.observation.IObservation;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.TupleResult;
import org.kalypso.ogc.gml.command.ChangeFeatureCommand;
import org.kalypso.ogc.gml.command.FeatureChange;
import org.kalypso.ogc.gml.featureview.IFeatureChangeListener;
import org.kalypso.ogc.gml.featureview.control.TupleResultFeatureControlWrapper;
import org.kalypso.ogc.gml.om.table.handlers.ComponentUiDateHandler;
import org.kalypso.ogc.gml.om.table.handlers.ComponentUiDoubleHandler;
import org.kalypso.ogc.gml.om.table.handlers.FixedComponentUIHandlerProvider;
import org.kalypso.ogc.gml.om.table.handlers.IComponentUiHandler;
import org.kalypsodeegree.model.feature.Feature;

/**
 * Wizard page with table of time series observation
 * 
 * @author Dirk Kuch
 */
public class PageEditTimeSeriesObservation extends WizardPage
{
  public static final String OBS_DATE = "urn:ogc:gml:dict:kalypso:wspm:sobek:boundaryConditionObservationDefs#DATE"; //$NON-NLS-1$

  QName QN_DATE = new QName( "http://www.w3.org/2001/XMLSchema", "dateTime" ); //$NON-NLS-1$ //$NON-NLS-2$

  protected List<FeatureChange> m_commands = new ArrayList<FeatureChange>();

  private final IBoundaryNodeLastfallCondition m_condition;

  public PageEditTimeSeriesObservation( final IBoundaryNodeLastfallCondition condition )
  {
    super( "editTimeSeriesObservation" ); //$NON-NLS-1$
    m_condition = condition;
    setTitle( Messages.PageEditTimeSeriesObservation_4 );
    setDescription( Messages.PageEditTimeSeriesObservation_5 );
  }

  /**
   * @see org.eclipse.jface.dialogs.IDialogPage#createControl(org.eclipse.swt.widgets.Composite)
   */
  public void createControl( final Composite parent )
  {
    setPageComplete( true );

    final Object layoutData = parent.getLayoutData();
    if( layoutData instanceof GridData )
    {
      final GridData pLayout = (GridData) layoutData;
      pLayout.widthHint = 900;
      pLayout.heightHint = 400;
      parent.layout();
    }

    final Composite container = new Composite( parent, SWT.NULL );
    container.setLayout( new GridLayout() );
    setControl( container );

    final IObservation<TupleResult> myObs = m_condition.getTimeSeriesObservation();
    final TupleResult tupleResult = myObs.getResult();

    final FixedComponentUIHandlerProvider provider = new FixedComponentUIHandlerProvider();

    final IComponent[] components = tupleResult.getComponents();
    for( int i = 0; i < components.length; i++ )
    {
      final IComponent component = components[i];

      final QName qname = component.getValueTypeName();

      IComponentUiHandler handler;
      if( QN_DATE.equals( qname ) )
        handler = new ComponentUiDateHandler( i, true, true, false, component.getName(), SWT.NONE, 100, 45, "%tF %tH:%tM", "%s", "" ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
      else
        handler = new ComponentUiDoubleHandler( i, true, true, false, component.getName(), SWT.NONE, 100, 45, "%s", "%s", "" ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$

      provider.add( i, handler );
    }

    // obsTable
    final TupleResultFeatureControlWrapper control = new TupleResultFeatureControlWrapper( m_condition.getTimeSeriesObservationFeature(), provider );
    control.draw( container, new GridData( GridData.FILL, GridData.FILL, true, true ) );

    control.addChangeListener( new IFeatureChangeListener()
    {
      public void featureChanged( final ICommand changeCommand )
      {
        if( changeCommand instanceof ChangeFeatureCommand )
        {
          final ChangeFeatureCommand chg = (ChangeFeatureCommand) changeCommand;
          final FeatureChange featureChange = chg.asFeatureChange();

          /* add feature change */
          m_commands.add( featureChange );
        }
      }

      public void openFeatureRequested( final Feature feature, final IPropertyType pt )
      {
      }
    } );
  }

  public FeatureChange[] getCommands( )
  {
    return m_commands.toArray( new FeatureChange[] {} );
  }
}
