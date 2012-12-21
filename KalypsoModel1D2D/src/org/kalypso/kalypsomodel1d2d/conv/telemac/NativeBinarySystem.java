/*
 * @creation 27 sept. 06
 * 
 * @modification $Date: 2006-09-28 13:39:23 $
 * 
 * @license GNU General Public License 2
 * 
 * @copyright (c)1998-2001 CETMEF 2 bd Gambetta F-60231 Compiegne
 * 
 * @mail devel@fudaa.fr
 */
package org.kalypso.kalypsomodel1d2d.conv.telemac;

import java.nio.ByteOrder;

/**
 * @author fred deniger
 * @version $Id: NativeBinarySystem.java,v 1.1 2006-09-28 13:39:23 deniger Exp $
 */
public class NativeBinarySystem {

  protected int l1_, l2_, l3_, l4_, l5_, l6_, l7_, l8_;
  protected int i1_, i2_, i3_, i4_;
  protected int s1_, s2_;

  private String machineType_;
  /**
   * L'identifiant pour les machine X86.
   */
  public final static String X86 = "86";
  /**
   * Le nom affiche.
   */
  public final static String X86_NAME = "X86";
  /**
   * L'identifiant pour les machines sparc.
   */
  public final static String SPARC = "sparc";
  /**
   * Le nom affiche.
   */
  public final static String SPARC_NAME = "SPARC";
  
  public ByteOrder byteOrder;

  public String getMachineType() {
    return machineType_;
  }

  public static final ByteOrder getByteOrder(final String _machine) {
    String machine = _machine;
    if (machine == null) {
      machine = NativeBinarySystem.getLocalMachine();
    }
    if (isX86(machine)) { return ByteOrder.LITTLE_ENDIAN; }
    return ByteOrder.BIG_ENDIAN;

  }

  public final void setMachineType(final String _machine) {
    String machine = _machine;
    if (machine == null) {
      machine = NativeBinarySystem.getLocalMachine();
    }
    byteOrder=getByteOrder(_machine);
    if (isX86(machine)) {
      machineType_ = X86;
      l1_ = 0;
      i1_ = 0;
      s1_ = 0;
      l2_ = 1;
      i2_ = 1;
      s2_ = 1;
      l3_ = 2;
      i3_ = 2;
      l4_ = 3;
      i4_ = 3;
      l5_ = 4;
      l6_ = 5;
      l7_ = 6;
      l8_ = 7;
    } else {
      machineType_ = SPARC;
      l1_ = 7;
      l2_ = 6;
      l3_ = 5;
      l4_ = 4;
      l5_ = 3;
      i1_ = 3;
      l6_ = 2;
      i2_ = 2;
      l7_ = 1;
      i3_ = 1;
      s1_ = 1;
      l8_ = 0;
      i4_ = 0;
      s2_ = 0;
    }
  }

  /**
   * @param _name le nom SPARC_NAME ou X86_NAME
   * @return null si non trouve
   */
  public static String getIdFromName(final String _name) {
    if (SPARC_NAME.equals(_name)) { return SPARC; }
    if (X86_NAME.equals(_name)) { return X86; }
    return null;
  }

  /**
   * @return le nom de la machine a partir des proprietes systemes
   */
  public static final String getLocalMachine() {
    return System.getProperty("os.arch");
  }

  /**
   * @return l'identifiant correspond a la machine.
   */
  public static final String getLocalMachineId() {
    return getMachineId(System.getProperty("os.arch"));
  }

  /**
   * Renvoie a partir de l'identifiant de la machine <code>_desc</code>, l'identifiant correctement géré par cette
   * classe. Si non trouvee, <code>null</code> est retourne. Les variables <code>SPARC</code> et <code>X86</code> sont
   * utilisees.
   * 
   * @param _desc la description de la machine
   * @return l'identifiant associe
   */
  public static final String getMachineId(final String _desc) {
    if (isX86(_desc)) {
      return X86;
    } else if (isSparc(_desc)) {
      return SPARC;
    } else {
      return null;
    }
  }

  /**
   * Si _id est egalt X86 renvoie X86_NAME et de meme pour sparc.
   * 
   * @param _id l'identifiant
   * @return le nom a afficher
   */
  public static final String getMachineName(final String _id) {
    if (X86.equals(_id)) { return X86_NAME; }
    if (SPARC.equals(_id)) { return SPARC_NAME; }
    return null;
  }

  /**
   * @return le nom SPARC_NAME ou X86_NAME de la machine local
   */
  public static final String getLocalMachineName() {
    return getMachineName(getLocalMachineId());
  }

  /**
   * @param _machine la description de la machine
   * @return true si <code>_machine</code> est du type X86 ( si la chaine finit par <code>"X86"</code>). La variable
   *         <code>X86</code> est utilisee.
   */
  public static final boolean isX86(final String _machine) {
    if (_machine == null) { return false; }
    if (_machine.endsWith(X86)) { return true; }
    return false;
  }

  /**
   * @param _machine la description de la machine
   * @return true, si <code>_machine</code> est un identifiant d'une machine sparc ( si finit par <code>"sparc"</code>).
   *         La variable <code>SPARC</code> est utilisee.
   */
  public static final boolean isSparc(final String _machine) {
    if (_machine == null) { return false; }
    if (_machine.endsWith(SPARC)) { return true; }
    return false;
  }

  /**
   * @param _machine l'identifiant ou la description de la machine
   * @return <code>true</code> si <code>_machine</code> est geree par cette classe.
   */
  public static boolean isMachineKnown(final String _machine) {
    return (isX86(_machine) || isSparc(_machine));
  }

  /**
   * @return the byteOrder
   */
  public ByteOrder getByteOrder() {
    return byteOrder;
  }

}
