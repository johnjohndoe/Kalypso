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
package org.kalypso.kalypso1d2d.pjt.wizards;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.viewers.ViewerFilter;
import org.eclipse.jface.wizard.IWizardPage;
import org.eclipse.swt.widgets.Composite;
import org.kalypso.kalypsomodel1d2d.conv.results.IRestartInfo;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.ICalcUnitResultMeta;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.IScenarioResultMeta;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.IStepResultMeta;
import org.kalypso.kalypsosimulationmodel.core.resultmeta.IResultMeta;
import org.kalypso.ui.wizards.results.SelectResultWizardPage;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapperCollection;

/**
 * @author Dejan Antanaskovic
 * 
 */
public class RestartSelectWizardPage1 extends SelectResultWizardPage
{
  private IScenarioResultMeta m_resultMeta;

  private List<IRestartInfo> m_restartInfos;

  public RestartSelectWizardPage1( final String pageName, final String title, final ImageDescriptor titleImage, final IScenarioResultMeta resultMeta, final List<IRestartInfo> restartInfos, final ViewerFilter filter )
  {
    super( pageName, title, titleImage, filter, null );
    m_resultMeta = resultMeta;
    m_restartInfos = restartInfos;
  }

  /**
   * @see org.kalypso.ui.wizards.results.SelectResultWizardPage#createControl(org.eclipse.swt.widgets.Composite)
   */
  @Override
  public void createControl( Composite parent )
  {
    super.createControl( parent );
    setInitialSelection();
  }

  public void setInitialSelection( )
  {
    final List<IResultMeta> checkedElements = new ArrayList<IResultMeta>();
    final List<IResultMeta> expandedElements = new ArrayList<IResultMeta>();
    for( final IRestartInfo restartInfo : m_restartInfos )
    {
      final ICalcUnitResultMeta calcUnitMetaResult = m_resultMeta.findCalcUnitMetaResult( restartInfo.getCalculationUnitID() );
      final String stepResultMetaID = restartInfo.getStepResultMetaID();
      final IFeatureWrapperCollection<IResultMeta> children = calcUnitMetaResult.getChildren();
      for( final IResultMeta child : children )
        if( child instanceof IStepResultMeta && child.getGmlID().equals( stepResultMetaID ) )
        {
          checkedElements.add( child );
          expandedElements.add( child.getParent() );
        }
    }
    m_treeViewer.setCheckedElements( checkedElements.toArray() );
    m_treeViewer.setExpandedElements( expandedElements.toArray() );
  }

  /**
   * @see org.eclipse.jface.wizard.WizardPage#getNextPage()
   */
  @Override
  public IWizardPage getNextPage( )
  {
    final IWizardPage nextPage = super.getNextPage();
    if( nextPage instanceof RestartSelectWizardPage2 )
      ((RestartSelectWizardPage2) nextPage).initializeResults( getSelectedResults() );
    return nextPage;
  }

  /**
   * @see org.eclipse.jface.wizard.WizardPage#canFlipToNextPage()
   */
  @Override
  public boolean canFlipToNextPage( )
  {
    return getSelectedResults().length > 0;
  }

}
