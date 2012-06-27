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
package org.kalypso.kalypsomodel1d2d.ui.map.calculation_unit;

import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.kalypso.contribs.eclipse.core.runtime.PluginUtilities;
import org.kalypso.contribs.eclipse.swt.widgets.DisposeButtonImageListener;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit1D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit1D2D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit2D;
import org.kalypso.kalypsomodel1d2d.ui.i18n.Messages;
import org.kalypso.kalypsomodel1d2d.ui.map.facedata.ICommonKeys;
import org.kalypso.kalypsomodel1d2d.ui.map.facedata.KeyBasedDataModelChangeListener;
import org.kalypso.ogc.gml.map.widgets.IWidgetWithStrategy;


/**
 * @author Madanagopal
 * @author Dejan Antanaskovic
 *
 */
public class CalculationUnitAdministerComponent
{
  private final CalculationUnitDataModel m_dataModel;

  protected static final ImageDescriptor IMAGEDATA_ADDREMOVE_ELEMENTS_1D = KalypsoModel1D2DPlugin.imageDescriptorFromPlugin( PluginUtilities.id( KalypsoModel1D2DPlugin.getDefault() ), "icons/elcl16/01_add_1D.gif" ); //$NON-NLS-1$

  protected static final ImageDescriptor IMAGEDATA_ADDREMOVE_ELEMENTS_2D = KalypsoModel1D2DPlugin.imageDescriptorFromPlugin( PluginUtilities.id( KalypsoModel1D2DPlugin.getDefault() ), "icons/elcl16/02_add_2D.gif" ); //$NON-NLS-1$

  protected static final ImageDescriptor IMAGEDATA_ADDREMOVE_CONTILINES = KalypsoModel1D2DPlugin.imageDescriptorFromPlugin( PluginUtilities.id( KalypsoModel1D2DPlugin.getDefault() ), "icons/elcl16/03_add_conti.gif" ); //$NON-NLS-1$

  protected static final ImageDescriptor IMAGEDATA_ADDREMOVE_BOUNDARYCONDITIONS = KalypsoModel1D2DPlugin.imageDescriptorFromPlugin( PluginUtilities.id( KalypsoModel1D2DPlugin.getDefault() ), "icons/elcl16/13_flowrelation_add_NodalBC.gif" ); //$NON-NLS-1$

  public CalculationUnitAdministerComponent( final CalculationUnitDataModel model )
  {
    m_dataModel = model;
  }

  public void createControl( final Composite parent )
  {
    final CalculationUnitDataModel dataModel = m_dataModel;

    final Composite rootComposite = new Composite( parent, SWT.FLAT );
    final GridLayout gridLayout = new GridLayout();
    gridLayout.numColumns = 4;
    gridLayout.makeColumnsEqualWidth = true;
    rootComposite.setLayout( gridLayout );

    final Button btnAddRemoveElements = new Button( rootComposite, SWT.PUSH );
    btnAddRemoveElements.setImage( IMAGEDATA_ADDREMOVE_ELEMENTS_1D.createImage() );
    DisposeButtonImageListener.hookToButton( btnAddRemoveElements );
    btnAddRemoveElements.setToolTipText( Messages.getString("org.kalypso.kalypsomodel1d2d.ui.map.calculation_unit.CalculationUnitAdministerComponent.0"));  //$NON-NLS-1$
    btnAddRemoveElements.addSelectionListener( new SelectionAdapter()
    {
      @Override
      public void widgetSelected( final SelectionEvent e )
      {
        final IWidgetWithStrategy widgetWithStrategy = (IWidgetWithStrategy) dataModel.getData( ICommonKeys.WIDGET_WITH_STRATEGY );
        widgetWithStrategy.setDelegate( new AddRemoveElementToCalcUnitWidget( dataModel ) );
      }
    } );

    final Button btnAddRemoveContinuityLines = new Button( rootComposite, SWT.PUSH );
    btnAddRemoveContinuityLines.setImage( IMAGEDATA_ADDREMOVE_CONTILINES.createImage() );
    DisposeButtonImageListener.hookToButton( btnAddRemoveContinuityLines );
    btnAddRemoveContinuityLines.setToolTipText(Messages.getString("org.kalypso.kalypsomodel1d2d.ui.map.calculation_unit.CalculationUnitAdministerComponent.1")) ;  //$NON-NLS-1$
    btnAddRemoveContinuityLines.addSelectionListener( new SelectionAdapter()
    {
      @Override
      public void widgetSelected( final SelectionEvent e )
      {
        final IWidgetWithStrategy widgetWithStrategy = (IWidgetWithStrategy) dataModel.getData( ICommonKeys.WIDGET_WITH_STRATEGY );
        widgetWithStrategy.setDelegate( new AddRemoveContinuityLineToCalcUnitWidget( dataModel ) );
      }
    } );

    final Button btnAddRemoveBoundaryConditions = new Button( rootComposite, SWT.PUSH );
    btnAddRemoveBoundaryConditions.setImage( IMAGEDATA_ADDREMOVE_BOUNDARYCONDITIONS.createImage() );
    DisposeButtonImageListener.hookToButton( btnAddRemoveBoundaryConditions );
    btnAddRemoveBoundaryConditions.setToolTipText(Messages.getString("org.kalypso.kalypsomodel1d2d.ui.map.calculation_unit.CalculationUnitAdministerComponent.2")) ;   //$NON-NLS-1$
    btnAddRemoveBoundaryConditions.addSelectionListener( new SelectionAdapter()
    {
      @Override
      public void widgetSelected( final SelectionEvent e )
      {
        final IWidgetWithStrategy widgetWithStrategy = (IWidgetWithStrategy) dataModel.getData( ICommonKeys.WIDGET_WITH_STRATEGY );
        widgetWithStrategy.setDelegate( new AddRemoveBoundaryConditionToCalcUnitWidget( dataModel ) );
      }
    } );

//    final Button btnAddRemoveWindSystems = new Button( rootComposite, SWT.PUSH );
//    btnAddRemoveWindSystems.setImage( IMAGEDATA_ADDREMOVE_BOUNDARYCONDITIONS.createImage() );
//    DisposeButtonImageListener.hookToButton( btnAddRemoveWindSystems );
//    btnAddRemoveWindSystems.setToolTipText(Messages.getString("Add remove wind systems to calculation unit")) ;
//    btnAddRemoveWindSystems.addSelectionListener( new SelectionAdapter()
//    {
//      @Override
//      public void widgetSelected( final SelectionEvent e )
//      {
//        final IWidgetWithStrategy widgetWithStrategy = (IWidgetWithStrategy) dataModel.getData( ICommonKeys.WIDGET_WITH_STRATEGY );
//        widgetWithStrategy.setStrategy( new AddRemoveWindSystemsToCalcUnitWidget( dataModel ) );
//      }
//    } );

    btnAddRemoveElements.setEnabled( false );
    btnAddRemoveContinuityLines.setEnabled( false );
    btnAddRemoveBoundaryConditions.setEnabled( false );

    m_dataModel.addKeyBasedDataChangeListener( new KeyBasedDataModelChangeListener()
    {
      @Override
      public void dataChanged( final String key, final Object newValue )
      {
        final Display display = parent.getDisplay();
        final Runnable runnable = new Runnable()
        {
          @Override
          public void run( )
          {
            if( ICommonKeys.KEY_SELECTED_FEATURE_WRAPPER.equals( key ) )
            {
              if( newValue == null )
              {
                btnAddRemoveElements.setEnabled( false );
                btnAddRemoveContinuityLines.setEnabled( false );
                btnAddRemoveBoundaryConditions.setEnabled( false );
//                btnAddRemoveWindSystems.setEnabled( false );
              }
              else
              {
                if( newValue instanceof ICalculationUnit1D )
                {
                  btnAddRemoveElements.getImage().dispose();
                  btnAddRemoveElements.setImage( IMAGEDATA_ADDREMOVE_ELEMENTS_1D.createImage() );
                  btnAddRemoveElements.setEnabled( true );
                  btnAddRemoveContinuityLines.setEnabled( true );
                  btnAddRemoveBoundaryConditions.setEnabled( true );
//                  btnAddRemoveWindSystems.setEnabled( false );
                }
                else if( newValue instanceof ICalculationUnit2D )
                {
                  btnAddRemoveElements.getImage().dispose();
                  btnAddRemoveElements.setImage( IMAGEDATA_ADDREMOVE_ELEMENTS_2D.createImage() );
                  btnAddRemoveElements.setEnabled( true );
                  btnAddRemoveContinuityLines.setEnabled( true );
                  btnAddRemoveBoundaryConditions.setEnabled( true );
//                  btnAddRemoveWindSystems.setEnabled( true );
                }
                else if( newValue instanceof ICalculationUnit1D2D )
                {
                  btnAddRemoveElements.setEnabled( false );
                  btnAddRemoveContinuityLines.setEnabled( false );
                  btnAddRemoveBoundaryConditions.setEnabled( true );
//                  btnAddRemoveWindSystems.setEnabled( true );
                }
                else
                {
                  btnAddRemoveElements.setEnabled( false );
                  btnAddRemoveContinuityLines.setEnabled( false );
                  btnAddRemoveBoundaryConditions.setEnabled( false );
//                  btnAddRemoveWindSystems.setEnabled( false );
                }
              }
            }
          }
        };
        display.syncExec( runnable );
      }
    } );
  }

}
