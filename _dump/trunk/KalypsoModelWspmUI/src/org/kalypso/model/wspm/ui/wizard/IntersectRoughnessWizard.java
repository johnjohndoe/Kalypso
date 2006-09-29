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
package org.kalypso.model.wspm.ui.wizard;

import org.eclipse.jface.wizard.Wizard;
import org.kalypso.contribs.eclipse.jface.wizard.ArrayChooserPage;
import org.kalypso.ui.editor.gmleditor.ui.GMLEditorLabelProvider2;
import org.kalypsodeegree.model.feature.Feature;

/**
 * @author Gernot Belger
 */
public class IntersectRoughnessWizard extends Wizard
{
  private final Feature[] m_features;

  private ArrayChooserPage m_chooserPage;

  private GMLEditorLabelProvider2 m_chooserPageLabelProvider = new GMLEditorLabelProvider2();

  public IntersectRoughnessWizard( final Feature[] features )
  {
    m_features = features;
  }

  /**
   * @see org.eclipse.jface.wizard.Wizard#addPages()
   */
  @Override
  public void addPages( )
  {
    /*
     * - page to choose polygon-data - page to choose assignment-gml - page to choose further parameters (welche fliesszone, ...)
     */

    m_chooserPage = new ArrayChooserPage( m_features, new Object[] {}, m_features, "profileFeaturesChooserPage", "Profile ausw‰hlen", null );
    m_chooserPage.setLabelProvider( m_chooserPageLabelProvider );
    m_chooserPage.setMessage( "Bitte w‰hlen Sie aus, welchen Profilen Rauheiten zugeweisen werden sollen." );

    addPage( m_chooserPage );

    super.addPages();
  }

  /**
   * @see org.eclipse.jface.wizard.Wizard#dispose()
   */
  @Override
  public void dispose( )
  {
    m_chooserPageLabelProvider.dispose();

    super.dispose();
  }
  
  /**
   * @see org.eclipse.jface.wizard.Wizard#performFinish()
   */
  @Override
  public boolean performFinish( )
  {
    final Object[] choosen = m_chooserPage.getChoosen();

    // intersect as wanted

    return true;
  }

}
