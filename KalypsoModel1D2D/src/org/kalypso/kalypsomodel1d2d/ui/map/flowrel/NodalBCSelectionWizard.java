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
package org.kalypso.kalypsomodel1d2d.ui.map.flowrel;

import java.lang.reflect.InvocationTargetException;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Date;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.PlatformUI;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.eclipse.jface.dialog.DialogSettingsUtils;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.contribs.eclipse.jface.operation.RunnableContextHelper;
import org.kalypso.gmlschema.GMLSchemaUtilities;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFELine;
import org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.IBoundaryCondition;
import org.kalypso.kalypsomodel1d2d.ui.i18n.Messages;
import org.kalypso.observation.IObservation;
import org.kalypso.observation.result.TupleResult;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.gml.selection.EasyFeatureWrapper;
import org.kalypso.ogc.gml.selection.IFeatureSelectionManager;
import org.kalypso.ui.editor.gmleditor.command.AddFeatureCommand;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.geometry.GM_Point;

/**
 * @author Dejan Antanaskovic, <a href="mailto:dejan.antanaskovic@tuhh.de">dejan.antanaskovic@tuhh.de</a>
 */
public class NodalBCSelectionWizard extends Wizard
{
  protected static final DateFormat DF = new SimpleDateFormat( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.flowrel.NodalBCSelectionWizard.0" ) ); //$NON-NLS-1$

  private NodalBCSelectionWizardPage m_selectionPage;

  private final IBoundaryConditionDescriptor[] m_descriptors;

  private final CommandableWorkspace m_workspace;

  private final IRelationType m_parentRelation;

  private final Feature m_parentFeature;

  private NodalBCDescriptorPage m_descriptorPage;

  private IBoundaryCondition m_boundaryCondition;

  private GM_Point m_boundaryPosition;

  private IFeatureSelectionManager m_selectionManager;

  private final Feature m_parentModelElement;

  /**
   * Construct a new instance and initialize the dialog settings for this instance.
   */
  public NodalBCSelectionWizard( final IBoundaryConditionDescriptor[] descriptors, final CommandableWorkspace workspace, final Feature parentFeature, final IRelationType parentRelation, final Feature parentModelElement )
  {
    m_descriptors = descriptors;
    m_workspace = workspace;
    m_parentFeature = parentFeature;
    m_parentRelation = parentRelation;
    m_parentModelElement = parentModelElement;
    setWindowTitle( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.flowrel.NodalBCSelectionWizard.1" ) ); //$NON-NLS-1$
    setDialogSettings( DialogSettingsUtils.getDialogSettings( KalypsoModel1D2DPlugin.getDefault(), "nodeBCselectionWizard" ) ); //$NON-NLS-1$
  }

  @Override
  public void addPages( )
  {
    m_descriptorPage = new NodalBCDescriptorPage( "descriptorPage", m_descriptors ); //$NON-NLS-1$
    m_selectionPage = new NodalBCSelectionWizardPage( "selectionPage", m_descriptors, m_descriptorPage ); //$NON-NLS-1$

    addPage( m_selectionPage );
    addPage( m_descriptorPage );
  }

  /**
   * This method is called by the wizard framework when the user presses the Finish button.
   */
  @Override
  public boolean performFinish( )
  {
    final IBoundaryConditionDescriptor descriptor = m_descriptorPage.getDescriptor();

    /* Create new feature */
    final IFeatureType newFT = GMLSchemaUtilities.getFeatureTypeQuiet( IBoundaryCondition.QNAME );
    final Feature newFeature = m_workspace.createFeature( m_parentFeature, m_parentRelation, newFT, -1 );
    final IBoundaryCondition bc = (IBoundaryCondition)newFeature.getAdapter( IBoundaryCondition.class );
    // System.out.println("PROP="+m_parentFeature.getProperty( m_parentRelation ));
    final ICoreRunnableWithProgress runnable = new ICoreRunnableWithProgress()
    {
      @Override
      @SuppressWarnings( "synthetic-access" )
      public IStatus execute( final IProgressMonitor monitor ) throws InvocationTargetException
      {
        bc.setName( descriptor.getName() );
        bc.setDescription( DF.format( new Date() ) );

        /* Initialize observation with components */
        final String domainComponentUrn = descriptor.getDomainComponentUrn();
        final String valueComponentUrn = descriptor.getValueComponentUrn();
        final IObservation<TupleResult> obs = bc.initializeObservation( domainComponentUrn, valueComponentUrn );
        descriptor.fillObservation( obs );
        bc.setObservation( obs );

        // isAbsolute property is available only for the BCs with element as a parent or as wave boundary condition
        if( m_parentModelElement instanceof IFELine )
          bc.setIsAbsolute( null );

        String lStrSteadyValue = m_selectionPage.getSteadyValue();
        if( !(descriptor instanceof WaveStepDescriptor) )
        {
          try
          {
            Double.parseDouble( lStrSteadyValue );
          }
          catch( final Exception e )
          {
            throw new IllegalArgumentException( "Illegal value set for stationary condition!" ); //$NON-NLS-1$
          }
        }
        else
        {
          lStrSteadyValue = WaveStepDescriptor.SWAN_BC_DEFAULT_STEADY_VALUE_PREFIX + lStrSteadyValue;
          bc.setIsAbsolute( ((WaveStepDescriptor)descriptor).isStateAbsoluteCond() );
        }

        bc.setStationaryCondition( lStrSteadyValue );
        bc.setParentElement( m_parentModelElement );

        if( m_boundaryPosition != null )
        {
          bc.setPosition( m_boundaryPosition );
          final AddFeatureCommand command = new AddFeatureCommand( m_workspace, m_parentFeature, m_parentRelation, -1, newFeature, m_selectionManager, true, true )
          {
            @Override
            public void process( ) throws Exception
            {
              super.process();
              if( m_selectionManager != null )
              {
                final EasyFeatureWrapper easyFeatureWrapper = new EasyFeatureWrapper( m_workspace, newFeature );

                try
                {
                  PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage().showView( "org.kalypso.featureview.views.FeatureView", null, IWorkbenchPage.VIEW_VISIBLE ); //$NON-NLS-1$
                  m_selectionManager.setSelection( new EasyFeatureWrapper[] { easyFeatureWrapper } );
                }
                catch( final Throwable pie )
                {
                  final IStatus status = StatusUtilities.statusFromThrowable( pie );
                  KalypsoModel1D2DPlugin.getDefault().getLog().log( status );
                  pie.printStackTrace();
                }
              }
            }
          };
          command.dropSelection( true );
          try
          {
            m_workspace.postCommand( command );
          }
          catch( final Throwable e )
          {
            e.printStackTrace();
          }
        }

        return Status.OK_STATUS;
      }
    };

    final IStatus status = RunnableContextHelper.execute( getContainer(), false, false, runnable );
    ErrorDialog.openError( getShell(), getWindowTitle(), Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.flowrel.NodalBCSelectionWizard.6" ), status ); //$NON-NLS-1$

    if( status.isOK() )
      m_boundaryCondition = bc;
    else
      KalypsoModel1D2DPlugin.getDefault().getLog().log( status );

    return status.isOK();
  }

  public IBoundaryCondition getBoundaryCondition( )
  {
    return m_boundaryCondition;
  }

  /**
   * Sets the target position of the boundary condition to be created.
   * 
   * @param boundaryPosition
   *          the target position
   */
  public void setBoundaryPosition( final GM_Point boundaryPosition )
  {
    m_boundaryPosition = boundaryPosition;
  }

  public void setSelectionManager( final IFeatureSelectionManager selectionManager )
  {
    m_selectionManager = selectionManager;
  }
}
