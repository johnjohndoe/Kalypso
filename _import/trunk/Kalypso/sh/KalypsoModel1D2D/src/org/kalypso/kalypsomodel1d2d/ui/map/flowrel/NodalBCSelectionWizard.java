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
import org.eclipse.jface.wizard.IWizard;
import org.eclipse.jface.wizard.Wizard;
import org.kalypso.contribs.eclipse.core.runtime.PluginUtilities;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.contribs.eclipse.jface.operation.RunnableContextHelper;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;
import org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.IBoundaryCondition;
import org.kalypso.observation.IObservation;
import org.kalypso.observation.result.TupleResult;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.geometry.GM_Point;

/**
 * @author Dejan Antanaskovic, <a href="mailto:dejan.antanaskovic@tuhh.de">dejan.antanaskovic@tuhh.de</a>
 */
public class NodalBCSelectionWizard extends Wizard implements IWizard
{
  protected static final DateFormat DF = new SimpleDateFormat( "'Manuell erzeugt am: 'dd.MM.yyyy H:mm" );

  private NodalBCSelectionWizardPage m_selectionPage;

  private final IBoundaryConditionDescriptor[] m_descriptors;

  private final GMLWorkspace/*CommandableWorkspace*/ m_workspace;

  private final IRelationType m_parentRelation;

  private final Feature m_parentFeature;

  private NodalBCDescriptorPage m_descriptorPage;

  private IBoundaryCondition m_boundaryCondition;
  
  private GM_Point boundaryPosition;

  /**
   * Construct a new instance and initialize the dialog settings for this instance.
   */
  public NodalBCSelectionWizard( 
            final IBoundaryConditionDescriptor[] descriptors, 
            final GMLWorkspace/*CommandableWorkspace*/ workspace, 
            final Feature parentFeature, 
            final IRelationType parentRelation )
  {
    m_descriptors = descriptors;
    m_workspace = workspace;
    m_parentFeature = parentFeature;
    m_parentRelation = parentRelation;
    setWindowTitle( "Randbedingung definieren" );
    setDialogSettings( PluginUtilities.getDialogSettings( KalypsoModel1D2DPlugin.getDefault(), "nodeBCselectionWizard" ) );
  }

  @Override
  public void addPages( )
  {
    m_descriptorPage = new NodalBCDescriptorPage( "descriptorPage", m_descriptors );
    m_selectionPage = new NodalBCSelectionWizardPage( "selectionPage", m_descriptors, m_descriptorPage );

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
    final IFeatureType newFT = m_workspace.getGMLSchema().getFeatureType( IBoundaryCondition.QNAME );
    final Feature newFeature = m_workspace.createFeature( m_parentFeature, m_parentRelation, newFT, -1 );
    final IBoundaryCondition bc = (IBoundaryCondition) newFeature.getAdapter( IBoundaryCondition.class );

    final ICoreRunnableWithProgress runnable = new ICoreRunnableWithProgress()
    {
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
        if( boundaryPosition != null )
        {
          bc.setPosition( boundaryPosition );
        }

        return Status.OK_STATUS;
      }
    };

    final IStatus status = RunnableContextHelper.execute( getContainer(), false, false, runnable );
    ErrorDialog.openError( getShell(), getWindowTitle(), "Fehler beim Erzeugen der Zeitreihe", status );

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
   * @param boundaryPosition the target position
   * 
   */
  public void setBoundaryPosition(GM_Point boundaryPosition)
  {
    this.boundaryPosition= boundaryPosition;
  }

}
