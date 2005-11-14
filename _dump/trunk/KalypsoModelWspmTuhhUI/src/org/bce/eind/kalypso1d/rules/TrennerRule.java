package org.bce.eind.kalypso1d.rules;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;

import com.bce.eind.core.ProfilCorePlugin;
import com.bce.eind.core.profil.IProfil;
import com.bce.eind.core.profil.IProfilConstants;
import com.bce.eind.core.profil.IProfilDevider;
import com.bce.eind.core.profil.ProfilDataException;
import com.bce.eind.core.profil.IProfilDevider.DEVIDER_TYP;
import com.bce.eind.core.profil.IProfilPoint.POINT_PROPERTY;
import com.bce.eind.core.profil.validator.AbstractValidatorRule;
import com.bce.eind.core.profil.validator.IValidatorMarkerCollector;

/**
 * Trennflächen und Bordvollpunkte dürfen nur innerhalb der durchströmten
 * Bereiche liegen
 * 
 * @author belger
 */
public class TrennerRule extends AbstractValidatorRule
{
  public void validate( final IProfil profil,
      final IValidatorMarkerCollector collector ) throws CoreException
  {
    if( profil == null )
      return;

    final IProfilDevider db[] = profil.getDevider( DEVIDER_TYP.DURCHSTROEMTE );
    final IProfilDevider tf[] = profil.getDevider( DEVIDER_TYP.TRENNFLAECHE );
    final IProfilDevider bv[] = profil.getDevider( DEVIDER_TYP.BORDVOLL );

    if( tf != null )
      validate( db, tf, profil, collector );
    if( bv != null )
      validate( db, bv, profil, collector );

  }

  private void validate( IProfilDevider[] db, IProfilDevider[] xx,
      final IProfil profil, final IValidatorMarkerCollector collector )
      throws CoreException
  {
    try
    {
      final double left = db[0].getPoint().getValueFor( POINT_PROPERTY.BREITE );
      final double right = db[1].getPoint().getValueFor( POINT_PROPERTY.BREITE );
      final double xleft = xx[0].getPoint().getValueFor( POINT_PROPERTY.BREITE );
      final double xright = xx[1].getPoint()
          .getValueFor( POINT_PROPERTY.BREITE );

      if( (xleft < left) || (xleft > right) )
      {
        collector.createProfilMarker( true, xx[0].getTyp().toString() + " ["
            + String.format( IProfilConstants.FMT_STATION,  xleft )
            + "] liegt außerhalb des Durchströmten Bereichs", "", 0, "" );
      }
      if( (xright < left) || (xright > right) )
      {
        collector.createProfilMarker( true, xx[1].getTyp().toString() + " ["
            + String.format( IProfilConstants.FMT_STATION,  xright )
            + "] liegt außerhalb des Durchströmten Bereichs", "", 0, "" );
      }
    }
    catch( ProfilDataException e )
    {
      e.printStackTrace();
      throw new CoreException( new Status( IStatus.ERROR, ProfilCorePlugin
          .getDefault().getBundle().getSymbolicName(), 0, "Profilfehler", e ) );
    }

  }
}
