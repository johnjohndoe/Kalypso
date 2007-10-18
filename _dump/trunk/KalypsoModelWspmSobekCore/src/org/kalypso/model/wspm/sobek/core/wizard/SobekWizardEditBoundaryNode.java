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
package org.kalypso.model.wspm.sobek.core.wizard;

import java.util.HashMap;
import java.util.Map;

import javax.xml.namespace.QName;

import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.ui.INewWizard;
import org.eclipse.ui.IWorkbench;
import org.kalypso.model.wspm.sobek.core.interfaces.IBoundaryNode;
import org.kalypso.model.wspm.sobek.core.interfaces.ISobekConstants;
import org.kalypso.model.wspm.sobek.core.wizard.pages.PageEditBoundaryNode;
import org.kalypso.ogc.gml.FeatureUtils;

/**
 * @author kuch
 */
public class SobekWizardEditBoundaryNode extends Wizard implements INewWizard
{

  private final IBoundaryNode m_boundaryNode;

  private PageEditBoundaryNode m_page;

  public SobekWizardEditBoundaryNode( final IBoundaryNode boundaryNode )
  {
    m_boundaryNode = boundaryNode;
  }

  /**
   * @see org.eclipse.ui.IWorkbenchWizard#init(org.eclipse.ui.IWorkbench,
   *      org.eclipse.jface.viewers.IStructuredSelection)
   */
  public void init( final IWorkbench workbench, final IStructuredSelection selection )
  {

  }

  /**
   * @see org.eclipse.jface.wizard.IWizard#addPages()
   */
  @Override
  public void addPages( )
  {
    m_page = new PageEditBoundaryNode( m_boundaryNode );
    addPage( m_page );
  }

  @Override
  public boolean performFinish( )
  {
    final Map<QName, Object> map = new HashMap<QName, Object>();
    map.put( ISobekConstants.QN_HYDRAULIC_NAME, m_page.getBoundaryName() );
    map.put( ISobekConstants.QN_HYDRAULIC_DESCRIPTION, m_page.getBoundaryDescription() );
    map.put( ISobekConstants.QN_HYDRAULIC_BOUNDARY_NODE_TYPE, m_page.getBoundaryType() );

    try
    {
      FeatureUtils.updateFeature( m_boundaryNode.getFeature(), map );
    }
    catch( final Exception e )
    {
      e.printStackTrace();

      return false;
    }

    return true;
  }
}
