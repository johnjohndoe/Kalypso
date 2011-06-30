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

import org.eclipse.core.resources.IFolder;
import org.eclipse.jface.window.Window;
import org.eclipse.jface.wizard.WizardDialog;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.handlers.IHandlerService;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DHelper;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.DiscretisationModelUtils;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IElement1D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DNode;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFELine;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IPolyElement;
import org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.IBoundaryCondition;
import org.kalypso.kalypsomodel1d2d.schema.dict.Kalypso1D2DDictConstants;
import org.kalypso.kalypsomodel1d2d.ui.i18n.Messages;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapper2;
import org.kalypsodeegree.model.geometry.GM_Point;

import de.renew.workflow.contexts.ICaseHandlingSourceProvider;

/**
 * 
 * @author Gernot Belger
 */
public class CreateBCFlowrelationWidget extends AbstractCreateFlowrelationWidget
{
  public CreateBCFlowrelationWidget( )
  {
    super( Messages.getString("org.kalypso.kalypsomodel1d2d.ui.map.flowrel.CreateBCFlowrelationWidget.0"), Messages.getString("org.kalypso.kalypsomodel1d2d.ui.map.flowrel.CreateBCFlowrelationWidget.1"), IBoundaryCondition.QNAME ); //$NON-NLS-1$ //$NON-NLS-2$
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.ui.map.flowrel.AbstractCreateFlowrelationWidget#createNewFeature(org.kalypso.ogc.gml.mapmodel.CommandableWorkspace,
   *      org.kalypsodeegree.model.feature.Feature, org.kalypso.gmlschema.property.relation.IRelationType,
   *      org.kalypso.gmlschema.feature.IFeatureType)
   */
  @Override
  protected IBoundaryCondition createNewFeature( final CommandableWorkspace workspace, final Feature parentFeature, final IRelationType parentRelation, final IFeatureWrapper2 modelElement )
  {
    final Display display = PlatformUI.getWorkbench().getDisplay();

    final IHandlerService handlerService = (IHandlerService) PlatformUI.getWorkbench().getService( IHandlerService.class );
    final IFolder scenarioFolder = (IFolder) handlerService.getCurrentState().getVariable( ICaseHandlingSourceProvider.ACTIVE_CASE_FOLDER_NAME );

    final IBoundaryConditionDescriptor[] descriptors = createTimeserieDescriptors( modelElement, scenarioFolder );

    /* Ask user for type of new feature */
    final NodalBCSelectionWizard wizard = new NodalBCSelectionWizard( descriptors, workspace, parentFeature, parentRelation, modelElement );

    final Shell shell = display.getActiveShell();
    final WizardDialog dialog = new WizardDialog( shell, wizard );
    if( dialog.open() == Window.CANCEL )
      return null;

    return wizard.getBoundaryCondition();
  }

  public static IBoundaryConditionDescriptor[] createTimeserieDescriptors( final IFeatureWrapper2 modelElement, final IFolder scenarioFolder )
  {
    final TimeserieStepDescriptor wstTimeDescriptor = new TimeserieStepDescriptor( Messages.getString("org.kalypso.kalypsomodel1d2d.ui.map.flowrel.CreateBCFlowrelationWidget.2"), Kalypso1D2DDictConstants.DICT_COMPONENT_TIME, Kalypso1D2DDictConstants.DICT_COMPONENT_WATERLEVEL ); //$NON-NLS-1$
    final TimeserieStepDescriptor qTimeDescriptor = new TimeserieStepDescriptor( Messages.getString("org.kalypso.kalypsomodel1d2d.ui.map.flowrel.CreateBCFlowrelationWidget.3"), Kalypso1D2DDictConstants.DICT_COMPONENT_TIME, Kalypso1D2DDictConstants.DICT_COMPONENT_DISCHARGE ); //$NON-NLS-1$
    final TimeserieStepDescriptor specQ1TimeDescriptor = new TimeserieStepDescriptor( Messages.getString("org.kalypso.kalypsomodel1d2d.ui.map.flowrel.CreateBCFlowrelationWidget.4"), Kalypso1D2DDictConstants.DICT_COMPONENT_TIME, Kalypso1D2DDictConstants.DICT_COMPONENT_SPECIFIC_DISCHARGE_1D ); //$NON-NLS-1$
    final TimeserieStepDescriptor specQ2TimeDescriptor = new TimeserieStepDescriptor( Messages.getString("org.kalypso.kalypsomodel1d2d.ui.map.flowrel.CreateBCFlowrelationWidget.5"), Kalypso1D2DDictConstants.DICT_COMPONENT_TIME, Kalypso1D2DDictConstants.DICT_COMPONENT_SPECIFIC_DISCHARGE_2D ); //$NON-NLS-1$
    final WQStepDescriptor wqDescriptor = new WQStepDescriptor( Messages.getString("org.kalypso.kalypsomodel1d2d.ui.map.flowrel.CreateBCFlowrelationWidget.6") ); //$NON-NLS-1$
    final WaveStepDescriptor waveDescriptor = new WaveStepDescriptor( "Wave Boundary Condition" ); 

    final IFolder importFolder = KalypsoModel1D2DHelper.getTimeeseriesFolder( scenarioFolder );
    final ZmlChooserStepDescriptor zmlChooser = new ZmlChooserStepDescriptor( Messages.getString("org.kalypso.kalypsomodel1d2d.ui.map.flowrel.CreateBCFlowrelationWidget.7"), importFolder ); //$NON-NLS-1$

    // TODO: ask ingenieurs what is right here:
    if( modelElement instanceof IElement1D )
      return new IBoundaryConditionDescriptor[] { specQ1TimeDescriptor, zmlChooser };
//    return new IBoundaryConditionDescriptor[] { specQ1TimeDescriptor, zmlChooser, wqDescriptor };

    if( modelElement instanceof IPolyElement )
      return new IBoundaryConditionDescriptor[] { specQ2TimeDescriptor, zmlChooser };
//    return new IBoundaryConditionDescriptor[] { specQ2TimeDescriptor, zmlChooser, wqDescriptor };

    // TODO: probably comment the next two lines out
    if( modelElement instanceof IFE1D2DNode )
      return new IBoundaryConditionDescriptor[] { wstTimeDescriptor, qTimeDescriptor, zmlChooser };
//    return new IBoundaryConditionDescriptor[] { wstTimeDescriptor, qTimeDescriptor, zmlChooser, wqDescriptor };

    if( modelElement instanceof IFELine )
      return new IBoundaryConditionDescriptor[] { wstTimeDescriptor, qTimeDescriptor, zmlChooser, wqDescriptor, waveDescriptor };

    return new IBoundaryConditionDescriptor[] {};
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.ui.map.flowrel.AbstractCreateFlowrelationWidget#findModelElementFromCurrentPosition(org.kalypso.kalypsomodel1d2d.schema.binding.IFEDiscretisationModel1d2d,
   *      org.kalypsodeegree.model.geometry.GM_Point, double)
   */
  @Override
  protected IFeatureWrapper2 findModelElementFromCurrentPosition( final IFEDiscretisationModel1d2d discModel, final GM_Point currentPos, final double grabDistance )
  {
    return DiscretisationModelUtils.findModelElementForBC( discModel, currentPos, grabDistance );
  }
}
