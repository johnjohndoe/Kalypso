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

import org.eclipse.jface.action.ToolBarManager;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.kalypso.commons.databinding.IDataBinding;
import org.kalypso.model.hydrology.binding.cm.ILinearSumGenerator;
import org.kalypso.model.hydrology.binding.cm.IMultiGenerator;
import org.kalypso.model.rcm.binding.IRainfallGenerator;
import org.kalypso.ui.rrm.internal.UIRrmImages;
import org.kalypso.ui.rrm.internal.UIRrmImages.DESCRIPTORS;
import org.kalypso.ui.rrm.internal.cm.view.action.DeleteGeneratorAction;
import org.kalypso.ui.rrm.internal.cm.view.action.EditGeneratorAction;
import org.kalypso.ui.rrm.internal.cm.view.action.EditLinearSumIdwAction;
import org.kalypso.ui.rrm.internal.cm.view.action.EditLinearSumThiessenAction;
import org.kalypso.ui.rrm.internal.i18n.Messages;
import org.kalypso.ui.rrm.internal.utils.featureBinding.FeatureBean;
import org.kalypso.ui.rrm.internal.utils.featureTree.AbstractTreeNodeUiHandler;
import org.kalypso.ui.rrm.internal.utils.featureTree.ITreeNodeModel;

/**
 * @author Gernot Belger
 */
public class GeneratorUiHandler extends AbstractTreeNodeUiHandler
{
  private final ITreeNodeModel m_model;

  private final IRainfallGenerator m_generator;

  public GeneratorUiHandler( final ITreeNodeModel model, final IRainfallGenerator generator )
  {
    m_model = model;
    m_generator = generator;
  }

  @Override
  public String getTypeLabel( )
  {
    return Messages.getString( "GeneratorUiHandler_0" ); //$NON-NLS-1$
  }

  @Override
  public String getTreeLabel( )
  {
    return m_generator.getDescription();
  }

  @Override
  public ImageDescriptor getTreeImage( )
  {
    if( m_generator instanceof ILinearSumGenerator )
      return UIRrmImages.id( DESCRIPTORS.GENERATOR_LINEAR_SUM );

    if( m_generator instanceof IMultiGenerator )
      return UIRrmImages.id( DESCRIPTORS.GENERATOR_MULTI );

    return null;
  }

  @Override
  protected Control createPropertiesControl( final Composite parent, final IDataBinding binding, final ToolBarManager sectionToolbar )
  {
    sectionToolbar.add( new EditGeneratorAction( m_model, m_generator ) );

    if( m_generator instanceof ILinearSumGenerator )
    {
      sectionToolbar.add( new EditLinearSumThiessenAction( m_model, (ILinearSumGenerator) m_generator ) );
      sectionToolbar.add( new EditLinearSumIdwAction( m_model, (ILinearSumGenerator) m_generator ) );
    }

    sectionToolbar.add( new DeleteGeneratorAction( m_generator ) );

    if( m_generator instanceof ILinearSumGenerator )
    {
      final FeatureBean<ILinearSumGenerator> bean = new FeatureBean<>( (ILinearSumGenerator)m_generator );
      return new LinearSumComposite( parent, bean, binding, false );
    }

    if( m_generator instanceof IMultiGenerator )
    {
      final FeatureBean<IMultiGenerator> bean = new FeatureBean<>( (IMultiGenerator)m_generator );
      return new MultiComposite( parent, bean, binding, false );
    }

    return new Composite( parent, SWT.NONE );
  }

  @Override
  protected void createHyperlinks( final FormToolkit toolkit, final Composite actionPanel )
  {
    // TODO
  }
}