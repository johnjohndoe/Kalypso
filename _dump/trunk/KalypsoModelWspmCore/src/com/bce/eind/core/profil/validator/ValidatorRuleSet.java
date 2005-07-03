package com.bce.eind.core.profil.validator;

import org.eclipse.core.resources.IMarker;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.WorkspaceJob;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;

import com.bce.eind.core.ProfilCorePlugin;
import com.bce.eind.core.profil.IProfil;

/**
 * A set of validation rules and the means to use them.
 * 
 * @author belger
 */
public class ValidatorRuleSet
{
  private final IValidatorRule[] m_rules;

  public ValidatorRuleSet( final IValidatorRule[] rules )
  {
    m_rules = rules;
  }

  public void validateProfile( final IProfil profil, final IResource resource )
  {
    final IValidatorRule[] rules = m_rules;

    final IValidatorMarkerCollector collector = new ResourceValidatorMarkerCollector( resource );
    
    final WorkspaceJob job = new WorkspaceJob( "Profil wird validiert" )
    {
      @Override
      public IStatus runInWorkspace( IProgressMonitor monitor ) throws CoreException
      {
        resource.deleteMarkers( IMarker.PROBLEM, false, IResource.DEPTH_ZERO );

        if( rules != null )
        {
          for( final IValidatorRule r : rules )
            r.validate( profil, collector );
        }

        return Status.OK_STATUS;
      }
    };
    job.schedule();
  }

  private static final class ResourceValidatorMarkerCollector implements IValidatorMarkerCollector
  {
    private final IResource m_resource;

    private final static String[] USED_ATTRIBUTES = new String[]
                                                         { IMarker.MESSAGE, IMarker.LOCATION, IMarker.SEVERITY, IValidatorMarkerCollector.MARKER_ATTRIBUTE_DATA };
    
    public ResourceValidatorMarkerCollector( final IResource resource )
    {
      m_resource = resource;
    }

    /**
     * Creates a (profile-)marker on the given resource. All validation rules should use this method,
     * so changes in the implementation (e.g. the type of the marker) are reflekted on all rules.
     * 
     * @throws CoreException
     */
    public void createProfilMarker( boolean isSevere,
        final String message, final String location, final Object data ) throws CoreException
    {
      final IMarker marker = m_resource.createMarker( ProfilCorePlugin.getID() + ".profilemarker" );

      final Object[] values = new Object[]
      { message, location, isSevere ? IMarker.SEVERITY_ERROR : IMarker.SEVERITY_WARNING, data };

      marker.setAttributes( USED_ATTRIBUTES, values );
    }
  }
}
