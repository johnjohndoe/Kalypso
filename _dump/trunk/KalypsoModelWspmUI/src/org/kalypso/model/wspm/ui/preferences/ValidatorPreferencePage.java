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
  private final static class ValidatorLabelProvider extends LabelProvider implements ITableLabelProvider 
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