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
package org.kalypso.kalypsomodel1d2d.ui.map.roughness_cor;

import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.FormAttachment;
import org.eclipse.swt.layout.FormData;
import org.eclipse.swt.layout.FormLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.eclipse.ui.forms.widgets.Section;

/**
 * Provide the ui for element wise roughness correction
 * 
 * @author Patrice Congo
 *
 */
public class ElWiseRoughnessCorrectionControl
{
  

  
  private final RoughnessCorrectionDataModel dataModel;
  
  private final FormToolkit toolkit;
  
  private final Section parentSection;

  public ElWiseRoughnessCorrectionControl(
              RoughnessCorrectionDataModel dataModel,
              FormToolkit toolkit, 
              Section parentSection)
  {
    this.dataModel = dataModel;
    this.toolkit = toolkit;
    this.parentSection = parentSection;    
  }
  
  public void createControl()
  {
    createFEStylingControlModel( parentSection );
  }
  
  public void dispose()
  {
    
  }
  
  /**
   * Creates a control which provide the interface to
   * control to roughness styling of elements
   * @param the parent section which provides client area
   *            to hold the styling control ui 
   *    
   */
  private final void createFEStylingControlModel( 
                                  Section workStatusSection )
  {
    
    workStatusSection.setLayout( new FillLayout() );

    Composite clientComposite = toolkit.createComposite( workStatusSection, SWT.FLAT );
    workStatusSection.setClient( clientComposite );
    FormLayout formLayout = new FormLayout();
    clientComposite.setLayout( formLayout );
    
    FormData formData;
    
    // Query Group
    formData = new FormData();
    formData.left = new FormAttachment( 0, 5 );
    formData.top = new FormAttachment( 0, 5 );
//    formData.bottom = new FormAttachment( 100, 0 );
    formData.right = new FormAttachment( 100, 0 );
    QueryGroup queryGroup = 
        new QueryGroup(dataModel,toolkit, clientComposite);   
    Control control = queryGroup.createControl();
    control.setLayoutData( formData );
  }
  

}
