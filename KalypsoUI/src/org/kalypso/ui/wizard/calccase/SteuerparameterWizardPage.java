package org.kalypso.ui.wizard.calccase;

import java.net.MalformedURLException;
import java.net.URL;
import java.util.ArrayList;
import java.util.Collection;

import org.deegree.model.feature.GMLWorkspace;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;
import org.kalypso.eclipse.core.resources.IProjectProvider;
import org.kalypso.java.lang.CatchRunnable;
import org.kalypso.ogc.gml.command.ChangeFeaturesCommand;
import org.kalypso.ogc.gml.featureview.FeatureChange;
import org.kalypso.ogc.gml.featureview.FeatureComposite;
import org.kalypso.ui.nature.ModelNature;

/**
 * Wizard-Page zur Eingabe der Steuerparameter
 * 
 * @author belger
 */
public class SteuerparameterWizardPage extends WizardPage
{
  private final IProjectProvider m_projectProvider;

  private GMLWorkspace m_controlGML = null;

  private FeatureComposite m_featureComposite;

  public SteuerparameterWizardPage( final String pageName, final IProjectProvider pp )
  {
    super( pageName, "Steurparameter", null );

    m_projectProvider = pp;
  }

  /**
   * @see org.eclipse.jface.dialogs.IDialogPage#createControl(org.eclipse.swt.widgets.Composite)
   */
  public void createControl( final Composite parent )
  {
    try
    {
      final IProject project = m_projectProvider.getProject();
      final ModelNature nature = (ModelNature)project.getNature( ModelNature.ID );
      m_controlGML = nature.getDefaultControl();

      // Vorlage auslesen
      final URL viewURL = new URL( "platform:/resource/" + project.getName() + "/"
          + ModelNature.CONTROL_VIEW_FILE );
      m_featureComposite = new FeatureComposite( m_controlGML.getRootFeature(), new URL[]
      { viewURL } );
      setControl( m_featureComposite.createControl( parent, SWT.NONE ) );
    }
    catch( final CoreException e )
    {
      e.printStackTrace();

      // TODO: error handling?
    }
    catch( final MalformedURLException e )
    {
      e.printStackTrace();
    }
  }

  public GMLWorkspace getGML() throws Exception
  {
    final Collection changes = new ArrayList();

    final FeatureComposite fc = m_featureComposite;
    final GMLWorkspace workspace = m_controlGML;
    
    getControl().getDisplay().syncExec( new CatchRunnable( )
    {
      public void runIntern() throws Exception
      {
        fc.collectChanges( changes );

        // Änderungen committen
        new ChangeFeaturesCommand( workspace, (FeatureChange[])changes
            .toArray( new FeatureChange[changes.size()] ) ).process();
        
      }
    } );
    
    return workspace;
  }
}