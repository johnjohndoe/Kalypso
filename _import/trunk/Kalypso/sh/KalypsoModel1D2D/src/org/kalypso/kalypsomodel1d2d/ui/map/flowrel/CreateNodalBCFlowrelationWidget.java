/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraße 22
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

import org.eclipse.core.expressions.IEvaluationContext;
import org.eclipse.core.resources.IFolder;
import org.eclipse.jface.window.Window;
import org.eclipse.jface.wizard.WizardDialog;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.kalypsomodel1d2d.schema.binding.IFE1D2DContinuityLine;
import org.kalypso.kalypsomodel1d2d.schema.binding.IFE1D2DNode;
import org.kalypso.kalypsomodel1d2d.schema.binding.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.schema.binding.IPolyElement;
import org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.IBoundaryCondition;
import org.kalypso.ogc.gml.map.widgets.IEvaluationContextConsumer;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ui.wizards.imports.ISzenarioSourceProvider;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapper2;
import org.kalypsodeegree.model.geometry.GM_Point;

/**
 * TODO: rename this class, its not nodal any more.
 * 
 * @author Gernot Belger
 */
public class CreateNodalBCFlowrelationWidget extends AbstractCreateFlowrelationWidget implements IEvaluationContextConsumer
{
  // TODO: move this to the Nodal...Wizard
  public final static class TimeserieTypeDescription
  {
    private final String m_name;

    private final String[] m_componentUrns;

    public TimeserieTypeDescription( final String name, final String... componentUrns )
    {
      m_name = name;
      m_componentUrns = componentUrns;
    }

    public String getName( )
    {
      return m_name;
    }

    public String[] getComponentUrns( )
    {
      return m_componentUrns;
    }
  }

  private IFolder m_currentScenarioFolder;

  public CreateNodalBCFlowrelationWidget( )
  {
    super( "Randbedingung erzeugen", "Randbedinung für einen FE-Knoten erzeugen", IBoundaryCondition.QNAME );
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.ui.map.flowrel.AbstractCreateFlowrelationWidget#createNewFeature(org.kalypso.ogc.gml.mapmodel.CommandableWorkspace,
   *      org.kalypsodeegree.model.feature.Feature, org.kalypso.gmlschema.property.relation.IRelationType,
   *      org.kalypso.gmlschema.feature.IFeatureType)
   */
  @Override
  protected IBoundaryCondition createNewFeature( final CommandableWorkspace workspace, final Feature parentFeature, final IRelationType parentRelation )
  {
    final Display display = PlatformUI.getWorkbench().getDisplay();

    final IBoundaryCondition[] bcresult = new IBoundaryCondition[1];

    display.syncExec( new Runnable()
    {
      public void run( )
      {
        /* Ask user for type of new feature */

        // TODO: make it dependend on the element type
        final TimeserieTypeDescription[] descriptions = getTimeserieDescriptions();

        final NodalBCSelectionWizard wizard = new NodalBCSelectionWizard( descriptions, workspace, parentFeature, parentRelation, m_currentScenarioFolder );

        final Shell shell = display.getActiveShell();
        final WizardDialog dialog = new WizardDialog( shell, wizard );
        if( dialog.open() == Window.CANCEL )
          return;

        // TODO
        // - maybe apply intervall
        // - maybe read from source

        bcresult[0] = wizard.getBoundaryCondition();
      }
    } );

    return bcresult[0];
  }

  protected TimeserieTypeDescription[] getTimeserieDescriptions( )
  {
    return new TimeserieTypeDescription[] {
        new TimeserieTypeDescription( "Wasserstand - Zeitreihe", "urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:components#Time", "urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:components#Waterlevel" ),
        new TimeserieTypeDescription( "Abfluss - Zeitreihe", "urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:components#Time", "urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:components#Discharge" ),
        new TimeserieTypeDescription( "Spezifische Abfluss - Zeitreihe", "urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:components#Time", "urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:components#SpecificDischarge1D" ),
        new TimeserieTypeDescription( "W/Q - Beziehung", "urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:components#Waterlevel", "urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:components#Discharge" ) };
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.ui.map.flowrel.AbstractCreateFlowrelationWidget#isConsidered(org.kalypsodeegree.model.feature.binding.IFeatureWrapper2)
   */
  @Override
  protected boolean isConsidered( final IFeatureWrapper2 modelElement )
  {
    return modelElement instanceof IFE1D2DNode || modelElement instanceof IFE1D2DContinuityLine || modelElement instanceof IPolyElement;
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.ui.map.flowrel.AbstractCreateFlowrelationWidget#findModelElementFromCurrentPosition(org.kalypso.kalypsomodel1d2d.schema.binding.IFEDiscretisationModel1d2d,
   *      org.kalypsodeegree.model.geometry.GM_Point, double)
   */
  @Override
  protected IFeatureWrapper2 findModelElementFromCurrentPosition( final IFEDiscretisationModel1d2d discModel, final GM_Point currentPos, final double grabDistance )
  {
    final IFE1D2DNode node = discModel.findNode( currentPos, grabDistance );
    if( node != null )
      return node;

    final IFE1D2DContinuityLine contiLine = discModel.findContinuityLine( currentPos, grabDistance / 2 );
    if( contiLine != null )
      return contiLine;

    final IPolyElement element2d = discModel.find2DElement( currentPos, grabDistance );
    return element2d;
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.IEvaluationContextConsumer#setEvaluationContext(org.eclipse.core.expressions.IEvaluationContext)
   */
  public void setEvaluationContext( IEvaluationContext evaluationContext )
  {
    m_currentScenarioFolder = (IFolder) evaluationContext.getVariable( ISzenarioSourceProvider.ACTIVE_SZENARIO_FOLDER_NAME );
  }
}
