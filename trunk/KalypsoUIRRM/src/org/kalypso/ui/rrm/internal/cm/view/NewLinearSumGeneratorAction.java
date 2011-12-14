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
package org.kalypso.ui.rrm.internal.cm.view;

import java.util.ArrayList;
import java.util.Collection;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.action.Action;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Shell;
import org.kalypso.afgui.scenarios.ScenarioHelper;
import org.kalypso.afgui.scenarios.SzenarioDataProvider;
import org.kalypso.model.hydrology.binding.model.Catchment;
import org.kalypso.model.hydrology.binding.model.NaModell;
import org.kalypso.model.rcm.binding.ILinearSumGenerator;
import org.kalypso.ui.rrm.internal.IUiRrmWorkflowConstants;
import org.kalypso.ui.rrm.internal.UIRrmImages;
import org.kalypso.ui.rrm.internal.UIRrmImages.DESCRIPTORS;
import org.kalypso.ui.rrm.internal.utils.featureTree.ITreeNodeModel;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;

/**
 * @author Gernot Belger
 */
public class NewLinearSumGeneratorAction extends Action
{
  private final ITreeNodeModel m_model;

  private final String m_parameterType;

  public NewLinearSumGeneratorAction( final ITreeNodeModel model, final String parameterType )
  {
    m_model = model;
    m_parameterType = parameterType;

    setText( "New Catchment Model (Linear Sum)" );
    setImageDescriptor( UIRrmImages.id( DESCRIPTORS.GENERATOR_NEW_LINEAR_SUM ) );
  }

  @Override
  public void runWithEvent( final Event event )
  {
    final Shell shell = event.widget.getDisplay().getActiveShell();

    final LinearSumBean bean = createEmptyBean();
    bean.setProperty( ILinearSumGenerator.PROPERTY_PARAMETER_TYPE, m_parameterType );

    final EditCatchmentsDialog dialog = new EditCatchmentsDialog( shell, m_model, bean );
    dialog.open();
  }

  /**
   * Create an empty bean with all catchments from the model gml.
   */
  private LinearSumBean createEmptyBean( )
  {
    try
    {
      final LinearSumBean bean = new LinearSumBean();

      final SzenarioDataProvider scenarioDataProvider = ScenarioHelper.getScenarioDataProvider();
      final NaModell model = scenarioDataProvider.getModel( IUiRrmWorkflowConstants.SCENARIO_DATA_MODEL, NaModell.class );
      final IFeatureBindingCollection<Catchment> catchments = model.getCatchments();

      final Collection<CatchmentBean> catchmentBeans = new ArrayList<>( catchments.size() );

      for( final Catchment catchment : catchments )
      {
        final CatchmentBean catchmentBean = new CatchmentBean();

        catchmentBean.setCatchmentRef( catchment.getId() );

        catchmentBeans.add( catchmentBean );
      }

      final CatchmentBean[] beans = catchmentBeans.toArray( new CatchmentBean[catchmentBeans.size()] );
      bean.setCatchments( beans );

      return bean;
    }
    catch( final CoreException e )
    {
      e.printStackTrace();
      // If this happens, it's a bug!
      return null;
    }
  }
}