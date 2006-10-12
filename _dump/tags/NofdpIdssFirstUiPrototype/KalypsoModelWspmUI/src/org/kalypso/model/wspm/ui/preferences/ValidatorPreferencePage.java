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
package org.kalypso.model.wspm.ui.preferences;

import org.eclipse.jface.preference.BooleanFieldEditor;
import org.eclipse.jface.preference.FieldEditorPreferencePage;
import org.eclipse.jface.viewers.ITableLabelProvider;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.swt.graphics.Image;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPreferencePage;
import org.kalypso.contribs.eclipse.jface.preference.ChecklistFieldEditor;
import org.kalypso.model.wspm.core.KalypsoModelWspmCorePlugin;
import org.kalypso.model.wspm.core.profil.validator.IValidatorRule;
import org.kalypso.model.wspm.ui.KalypsoModelWspmUIPlugin;


/**
 * @author gernot
 * 
 */
public class ValidatorPreferencePage extends FieldEditorPreferencePage implements
    IWorkbenchPreferencePage
{
  protected final static class ValidatorLabelProvider extends LabelProvider implements ITableLabelProvider 
  {
    public Image getColumnImage( Object element, int columnIndex )
    {
      return null;
    }

    public String getColumnText( final Object element, int columnIndex )
    {
      if( element instanceof IValidatorRule )
        return ((IValidatorRule)element).getDescription();
      
      return "";
    }
  }
  
  public ValidatorPreferencePage( )
  {
    super( GRID );
    
    setPreferenceStore( KalypsoModelWspmUIPlugin.getDefault().getPreferenceStore() );
    
    setDescription( "Profil Validierung" );
  }

  @Override
  public void createFieldEditors( )
  {
    addField( new BooleanFieldEditor( PreferenceConstants.P_VALIDATE_PROFILE,
        "Profile &validieren", getFieldEditorParent() ) );

    final Object[] elements = KalypsoModelWspmCorePlugin.getDefault().getValidatorFactory().getAllRules();
    addField( new ChecklistFieldEditor( elements, "getID", new ValidatorLabelProvider(),  PreferenceConstants.P_VALIDATE_RULES_TO_EXCLUDE, "", getFieldEditorParent() ) );
  }

  /**
   * @see org.eclipse.ui.IWorkbenchPreferencePage#init(org.eclipse.ui.IWorkbench)
   */
  public void init( IWorkbench workbench )
  {
  }
}