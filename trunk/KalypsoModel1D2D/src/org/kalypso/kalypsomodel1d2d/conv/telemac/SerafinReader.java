/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraße 22
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
/**
 * @creation 22 nov. 2004
 * @modification $Date: 2007-01-19 13:07:22 $
 * @license GNU General Public License 2
 * @copyright (c)1998-2001 CETMEF 2 bd Gambetta F-60231 Compiegne
 * @mail devel@fudaa.fr
 */
package org.kalypso.kalypsomodel1d2d.conv.telemac;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.nio.ByteBuffer;
import java.nio.channels.FileChannel;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collections;
import java.util.EnumMap;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.deegree.framework.util.Pair;
import org.kalypso.kalypsomodel1d2d.conv.results.NodeResultHelper;
import org.kalypso.kalypsomodel1d2d.conv.results.NodeResultsHandler;
import org.kalypsodeegree.model.geometry.GM_Position;

/**
 * @author ig, based on SerafinNewReader created by Fred Deniger
 * @version $Id: SerafinNewReader.java,v 1.17 2007-01-19 13:07:22 deniger Exp $
 */
public class SerafinReader
{
  public static enum VAR_NAMES
  {
    BOTTOM,
    VEL_U,
    VEL_V,
    WD,
    WL
  }

  public static final String BOTTOM_En = "BOTTOM"; //$NON-NLS-1$

  public static final String BOTTOM_Fr = "FOND"; //$NON-NLS-1$

  public static final String VEL_U_En = "VELOCITY U"; //$NON-NLS-1$

  public static final String VEL_U_Fr = "VITESSE U"; //$NON-NLS-1$

  public static final String VEL_V_En = "VELOCITY V"; //$NON-NLS-1$

  public static final String VEL_V_Fr = "VITESSE V"; //$NON-NLS-1$

  public static final String WD_En = "WATER DEPTH"; //$NON-NLS-1$

  public static final String WD_Fr = "HAUTEUR D'EAU"; //$NON-NLS-1$

  public static final String WL_En = "FREE SURFACE"; //$NON-NLS-1$

  public static final String WL_Fr = "SURFACE LIBRE"; //$NON-NLS-1$

  public static final int IPARAM_NB = SerafinWriter.IPARAM_NB;

//  /**
//   * @return the isVolumique
//   */
//  public boolean isVolumique( )
//  {
//    return isVolumique_;
//  }
//
//  /**
//   * @param _isVolumique
//   *          the isVolumique to set
//   */
//  public void setVolumique( boolean _isVolumique )
//  {
//    isVolumique_ = _isVolumique;
//  }

  FileInputStream m_in;

  NativeNIOHelper m_helper;

  File m_file;

//  boolean isVolumique_;

  boolean m_onlyReadLast = false;

  long readTimeStepFrom_ = -1;

  private String m_header;

  private NodeResultsHandler m_nodesHandler;

  private boolean m_precisionDouble = false;

  @SuppressWarnings( "unused" )
  private long m_startDateTime = 0;

  private Map<Integer, Pair<String, Map<String, List<Double>>>> m_resMap;

  private List<int[]> m_trianglesNodes;

  private List<GM_Position> m_listPoses;

  private String[] m_namesVariables;

  private String[] m_uniteVariables;

  private Integer m_index = null;

  private int[] m_iparams;

  private int m_nelem1;

  private int m_npoin1;

  private int m_nppel1;

  private int m_nbv1;

  private int m_countStep = 0;

  public SerafinReader( )
  {
//    this( false );
  }

  /**
   * @return true si on doit lire que le dernier pas de temps
   */
  public final boolean isOnlyReadLast( )
  {
    return m_onlyReadLast;
  }

  /**
   * @param _onlyReadLast
   *          true si on doit lire que le dernier pas de temps
   */
  public final void setOnlyReadLast( final boolean onlyReadLast )
  {
    m_onlyReadLast = onlyReadLast;
  }

  public static boolean isIdateDefiniCommon( final int[] s )
  {
    if( s[9] == 1 )
    {
      return true;
    }
    return false;
  }

  public static boolean isFormatEnColonneCommon( final int[] s )
  {
    return s[0] == 1 ? true : false;
  }

  public boolean readHeader( ) throws IOException
  {
    m_helper = new NativeNIOHelper( m_in.getChannel() );
    m_resMap = new HashMap<>();
    m_listPoses = new ArrayList<>();
    final List<Float> listX = new ArrayList<>();
    final List<Float> listY = new ArrayList<>();

    // read first 88 bytes, the first integer should be 80
    m_helper.readAll( 88 );
    ByteBuffer bf = m_helper.getBuffer();
    int tempInt = bf.getInt();
    // if it not 80 then we invert order
    if( tempInt != 80 )
    {
      m_helper.inverseOrder();
    }
    // if still not - bad file format
    tempInt = bf.getInt( 0 );
    if( tempInt != 80 )
    {
      throw new IOException( "Invalid serafin file format!" ); //$NON-NLS-1$
    }
    // read title
    m_header = m_helper.getStingFromBuffer( 80 ).trim();
    if( m_header.toLowerCase().endsWith( "d" ) ) //$NON-NLS-1$
    {
      m_precisionDouble = true;
    }
    // NBV1,NBV2
    m_helper.readData();
    bf = m_helper.getBuffer();
    m_nbv1 = bf.getInt();
    // if nbv1 negative also bad file format
    if( m_nbv1 < 0 )
    {
      throw new IOException( "Invalid serafin file!" );
//        return null;
    }
    // variable of second discretisation usually used
    final int nbv2 = bf.getInt();
    // warning
    if( nbv2 > 0 )
    {
//        analyze_.addWarn( EfIOResource.getS( "Les variables de seconde discretisation seront ignorées" ), -1 );
    }
    m_namesVariables = isReadOnlyTimeStep() ? null : new String[m_nbv1];
    m_uniteVariables = isReadOnlyTimeStep() ? null : new String[m_nbv1];
    // int nbOctet = helper_.getLengthRecordForChar(32);
    for( int i = 0; i < m_nbv1; i++ )
    {
      m_helper.readData();
      if( !isReadOnlyTimeStep() )
      {
        bf = m_helper.getBuffer();
        m_namesVariables[i] = m_helper.getStingFromBuffer( 16 ).trim();
        m_uniteVariables[i] = m_helper.getStingFromBuffer( 16 ).trim();
      }
    }
    // in case of nbv2 != 0
    for( int i = 0; i < nbv2; i++ )
    {
      m_helper.skipRecord();
    }
    // IPARAM
    // read the table
    final int nbParam = IPARAM_NB;
    m_iparams = new int[nbParam];
    m_helper.readData();
    bf = m_helper.getBuffer();
    for( int i = 0; i < nbParam; i++ )
    {
      m_iparams[i] = bf.getInt();
    }

    // Idate
    if( isIdateDefiniCommon( m_iparams ) )
    {
      bf = m_helper.readData();
      if( !isReadOnlyTimeStep() )
      {
        // Annee, mois, jour, heure, minute, seconde
        final int y = bf.getInt();
        final int m = bf.getInt();
        final int j = bf.getInt();
        final int h = bf.getInt();
        final int min = bf.getInt();
        final int s = bf.getInt();
        final Calendar cal = Calendar.getInstance();
        cal.set( Calendar.YEAR, y );
        cal.set( Calendar.MONTH, m );
        cal.set( Calendar.DAY_OF_MONTH, j );
        cal.set( Calendar.HOUR_OF_DAY, h );
        cal.set( Calendar.MINUTE, min );
        cal.set( Calendar.SECOND, s );
        cal.set( Calendar.MILLISECOND, 0 );
//          inter.setIdateInMillis( cal.getTime().getTime() );
        m_startDateTime = cal.getTime().getTime();
      }
    }

    // NELEM1,NPOIN1,NPPEL1,IDISC1
    bf = m_helper.readData();
    m_nelem1 = bf.getInt();
    m_npoin1 = bf.getInt();
    m_nppel1 = bf.getInt();
    @SuppressWarnings( "unused" ) int idisc = bf.getInt();

    m_trianglesNodes = new ArrayList<>();

    // if nbv2 > 0 then ignore
    if( nbv2 > 0 )
    {
      m_helper.skipRecord();
    }
    bf = m_helper.readData();
    int[] indexElem;
    if( !isReadOnlyTimeStep() )
    {
      for( int i = 0; i < m_nelem1; i++ )
      {
        indexElem = new int[m_nppel1];
        for( int j = 0; j < m_nppel1; j++ )
        {

          indexElem[j] = bf.getInt() - 1;
        }
        m_trianglesNodes.add( indexElem );
      }
    }
    /*
     * Read the table of correspondence for the second discretisation if needed
     */
    if( nbv2 > 0 )
    {
      m_helper.skipRecord();
    }
    // table of indicators of the points
    int[] ipobo1 = isReadOnlyTimeStep() ? null : new int[m_npoin1];
    int[] ipoboInit = isReadOnlyTimeStep() ? null : new int[m_npoin1];

    bf = m_helper.readData();
    int temp;
    int index = 0;
    if( !isReadOnlyTimeStep() )
    {
      for( int i = 0; i < m_npoin1; i++ )
      {
        // store only the border points which are only positive
        temp = bf.getInt();
        ipoboInit[i] = temp;
        if( temp > 0 )
        {
          index++;
          if( temp < m_npoin1 )
          {
            ipobo1[temp - 1] = i;
          }
        }
      }
      final int[] tempipobo1 = new int[index];
      System.arraycopy( ipobo1, 0, tempipobo1, 0, index );

    }
    // ignore...
    if( nbv2 > 0 )
    {
      m_helper.skipRecord();
    }
    bf = m_helper.readData();
    if( !isReadOnlyTimeStep() )
    {
      for( int i = 0; i < m_npoin1; i++ )
      {
        float x = bf.getFloat();
        listX.add( x );
      }
    }
    bf = m_helper.readData();
    if( !isReadOnlyTimeStep() )
    {
      for( int i = 0; i < m_npoin1; i++ )
      {
        float y = bf.getFloat();
        listY.add( y );
      }
    }
    m_listPoses = NodeResultHelper.createListOfPositions( listX, listY, new Integer[] { m_iparams[2], m_iparams[3] } );
    // ignore
    if( nbv2 > 0 )
    {
      // x2
      m_helper.skipRecord();
      // y2
      m_helper.skipRecord();
    }
    // read timestep
    return true;
  }
  
  public Map<String, List<Double>> doReadStep( int index ) throws IOException{
    int tempo;
    ByteBuffer bf = m_helper.getBuffer();
    int nbPtOrElt = m_npoin1;
    if( isFormatEnColonneCommon( m_iparams ) )
    {
      /*
       * for column we have TEMPS: TEMPS+4 variable1 : 4+NPPOIN14+4 variablei : 4+NPPOIN14+4
       * for each variable wich means: tempo= 4+4+Nbv1(4+NPPOINT4+4)
       */
      tempo = 12 + 4 * m_nbv1 * nbPtOrElt + 8 * m_nbv1;
    }
    else
    {
      /*
       * Temps,variable1,variablei,... means 4+(NPPOIN14)Nbv1+4
       */
      tempo = 12 + 4 * m_nbv1 * nbPtOrElt;
    }

    // number of remaining bytes to read divided by the number of all bytes in timestep
    final int nbPasTempsEstime = (int)(m_helper.getAvailable() / tempo);

    // the read record is already done
    // tempo stores the number of timesteps really readed
    // int nbPasTemps = 0;
    
    if( m_nbv1 > 0 )
    {
      m_helper.readSequentialData();
      final FileChannel ch = m_helper.getChannel();
      if( m_onlyReadLast )
      {
        ch.position( ch.position() + tempo * (nbPasTempsEstime - 1) );
      }
      else if( isReadOnlyTimeStep() )
      {
        ch.position( ch.position() + (tempo * (this.readTimeStepFrom_)) );
      }
      // on va lire que les valeurs des pas de temps.
//      int iCountStep = 0;
      int iCountVariable = 0;
      int iCountValue = 0;
      Map<String, List<Double>> mapResStep = new HashMap<>();
      List<Double> listVarVal = new ArrayList<>();
      m_helper.readAll( 4 );
      bf = m_helper.getBuffer();
      Float stepTime = bf.getFloat();
      m_startDateTime = stepTime.longValue();
      skipRecords( 2 );
      while( true )
      {
        if( iCountValue == m_listPoses.size() )
        {
          mapResStep.put( m_namesVariables[iCountVariable], listVarVal );
          iCountValue = 0;
          iCountVariable++;
          listVarVal = new ArrayList<>();
          skipRecords( 2 );
        }
        if( iCountVariable == m_namesVariables.length )
        {
          System.out.println( "put: " + stepTime );
          m_resMap.put( m_countStep, new Pair<>( "" + stepTime, mapResStep ) ); //$NON-NLS-1$
          if( m_helper.getAvailable() > 0 )
          {
            m_helper.readAll( 4 );
            bf = m_helper.getBuffer();
            stepTime = bf.getFloat();
            skipRecords( 2 );
          }
          return mapResStep;
//          m_countStep ++;
//          mapResStep = new HashMap<>();
//          iCountVariable = 0;
        }
        if( m_helper.getAvailable() <= 0 )
        {
          break;
        }
        // TEMPS
        // utilisation de la variable temporaire
        // on lit un float sur 4 bytes

        // use of temporary variable, we read floats( 4 bytes )
        m_helper.readAll( 4 );
        bf = m_helper.getBuffer();
        listVarVal.add( readFloatingPointValue( bf ) );
        m_countStep++;
      }
    }
    return null;
  }

  public Map<Integer, Pair<String, Map<String, List<Double>>>> doReadAll( ) throws IOException
  {
    m_helper = new NativeNIOHelper( m_in.getChannel() );
    m_resMap = new HashMap<>();
    m_listPoses = new ArrayList<>();
    final List<Float> listX = new ArrayList<>();
    final List<Float> listY = new ArrayList<>();
    try
    {
      // on lit les 88 premiers octets. On sait que le premier entier doit etre 80.
      // read first 88 bytes, the first integer should be 80
      m_helper.readAll( 88 );
      ByteBuffer bf = m_helper.getBuffer();
      int tempInt = bf.getInt();
      // le premier entier ne vaut pas 80: on inverse l'ordre de lecture
      // if it not 80 then we invert order
      if( tempInt != 80 )
      {
        m_helper.inverseOrder();
      }
      // toujours pas ... ce n'est pas un fichier serafin correct
      // if still not - bad file format
      tempInt = bf.getInt( 0 );
      if( tempInt != 80 )
      {
        throw new IOException( "Invalid serafin file format!" ); //$NON-NLS-1$
      }
      // on lit le titre
      // read title
      m_header = m_helper.getStingFromBuffer( 80 ).trim();
      if( m_header.toLowerCase().endsWith( "d" ) ) //$NON-NLS-1$
      {
        m_precisionDouble = true;
      }
//      System.out.println( m_header );
      // NBV1,NBV2
      m_helper.readData();
      bf = m_helper.getBuffer();
      final int nbv1 = bf.getInt();
      // une erreure plus qu'anormale: on renvoie null
      // if nbv1 negative also bad file format
      if( nbv1 < 0 )
      {
        throw new IOException( "Invalid serafin file!" );
//        return null;
      }
      // variables de secondes discretisation : plus utilisees.
      // variable of second discretisation usually used
      final int nbv2 = bf.getInt();
      // un avertissement
      // warning
      if( nbv2 > 0 )
      {
//        analyze_.addWarn( EfIOResource.getS( "Les variables de seconde discretisation seront ignorées" ), -1 );
      }
      m_namesVariables = isReadOnlyTimeStep() ? null : new String[nbv1];
      m_uniteVariables = isReadOnlyTimeStep() ? null : new String[nbv1];
      // int nbOctet = helper_.getLengthRecordForChar(32);
      for( int i = 0; i < nbv1; i++ )
      {
        m_helper.readData();
        if( !isReadOnlyTimeStep() )
        {
          bf = m_helper.getBuffer();
          m_namesVariables[i] = m_helper.getStingFromBuffer( 16 ).trim();
          m_uniteVariables[i] = m_helper.getStingFromBuffer( 16 ).trim();
        }
      }
//      inter.setNomVariables( nomVariables );
//      inter.setUniteVariables( uniteVariables );

      // Au cas ou NBV2 soit non nulle.
      // in case of nbv2 != 0
      for( int i = 0; i < nbv2; i++ )
      {
        m_helper.skipRecord();
      }
      // IPARAM
      // Lecture du tableau
      // read the table
      final int nbParam = IPARAM_NB;
      final int[] iparam = new int[nbParam];
      m_helper.readData();
      bf = m_helper.getBuffer();
      for( int i = 0; i < nbParam; i++ )
      {
        iparam[i] = bf.getInt();
      }

      // Idate
      if( isIdateDefiniCommon( iparam ) )
      {
        bf = m_helper.readData();
        if( !isReadOnlyTimeStep() )
        {
          // Annee, mois, jour, heure, minute, seconde
          final int y = bf.getInt();
          final int m = bf.getInt();
          final int j = bf.getInt();
          final int h = bf.getInt();
          final int min = bf.getInt();
          final int s = bf.getInt();
          final Calendar cal = Calendar.getInstance();
          cal.set( Calendar.YEAR, y );
          cal.set( Calendar.MONTH, m );
          cal.set( Calendar.DAY_OF_MONTH, j );
          cal.set( Calendar.HOUR_OF_DAY, h );
          cal.set( Calendar.MINUTE, min );
          cal.set( Calendar.SECOND, s );
          cal.set( Calendar.MILLISECOND, 0 );
//          inter.setIdateInMillis( cal.getTime().getTime() );
          m_startDateTime = cal.getTime().getTime();
        }
      }

      // NELEM1,NPOIN1,NPPEL1,IDISC1
      bf = m_helper.readData();
      final int nelem1 = bf.getInt();
      final int npoin1 = bf.getInt();
      final int nppel1 = bf.getInt();
      @SuppressWarnings( "unused" ) int idisc = bf.getInt();

      m_trianglesNodes = new ArrayList<>();

      // Si nbv2>0 on ignore
      // if nbv2 > 0 then ignore
      if( nbv2 > 0 )
      {
        m_helper.skipRecord();
      }
      bf = m_helper.readData();
      int[] indexElem;
      if( !isReadOnlyTimeStep() )
      {
        for( int i = 0; i < nelem1; i++ )
        {
          indexElem = new int[nppel1];
          for( int j = 0; j < nppel1; j++ )
          {

            indexElem[j] = bf.getInt() - 1;
          }
          m_trianglesNodes.add( indexElem );
        }
      }
      /*
       * Lecture du tableau de correspondance pour la 2eme discrétisation Si nécessaire
       * Read the table of correspondence for the second discretisation if needed
       */
      if( nbv2 > 0 )
      {
        m_helper.skipRecord();
      }
      // tableau d'indicateurs de points de bords
      // table of indicators of the points
      int[] ipobo1 = isReadOnlyTimeStep() ? null : new int[npoin1];
      int[] ipoboInit = isReadOnlyTimeStep() ? null : new int[npoin1];

      bf = m_helper.readData();
      int temp;
      int index = 0;
      if( !isReadOnlyTimeStep() )
      {
        for( int i = 0; i < npoin1; i++ )
        {
          // on stocke seulement les points frontiere indice strictement positif
          // store only the border points which are only positive
          temp = bf.getInt();
          ipoboInit[i] = temp;
          if( temp > 0 )
          {
            index++;
            // represente l'indice -1
            //
            if( temp < npoin1 )
            {
              ipobo1[temp - 1] = i;
            }
          }
        }
        final int[] tempipobo1 = new int[index];
        System.arraycopy( ipobo1, 0, tempipobo1, 0, index );

      }
      // ignore...
      if( nbv2 > 0 )
      {
        m_helper.skipRecord();
      }
      bf = m_helper.readData();
      if( !isReadOnlyTimeStep() )
      {
        for( int i = 0; i < npoin1; i++ )
        {
          float x = bf.getFloat();
          listX.add( x );
        }
      }
      bf = m_helper.readData();
      if( !isReadOnlyTimeStep() )
      {
        for( int i = 0; i < npoin1; i++ )
        {
          float y = bf.getFloat();
          listY.add( y );
        }
      }
      m_listPoses = NodeResultHelper.createListOfPositions( listX, listY, new Integer[] { iparam[2], iparam[3] } );
      // ignore
      if( nbv2 > 0 )
      {
        // x2
        m_helper.skipRecord();
        // y2
        m_helper.skipRecord();
      }
      // Lecture des pas de temps.
      // read timestep
      int tempo;
      /*
       * Calcul du nb de pas de temps a partir des octets restants à lire. la variable tempo stocke le nb d'octets
       * utilise pour chaque pas de temps. Il y a NPPOIN1 entier par variable et Nbv1 variable. Rappel:les entiers
       * prennent 4 octets. Rappel:chaque enregistrement est entoure par des entiers d'ou les 4
       * calculate the number of timesteps based on remaining bytes. The variable "tempo" stores the number of bytes used for each timestep.
       * there is NPPOIN1 integers for each variable and nbv1 variables.
       * Reminder: the integers are 4 bytes. Each record alligned to 4 bytes
       */
      // TODO fred ici comment savoir si volumique ou resultat aux noeud.
      // how to diffirinciate between 2d und 3d(volumique?) results
//      int nbPtOrElt = isVolumique_ ? nelem1 : npoin1;
      int nbPtOrElt = npoin1;
      if( isFormatEnColonneCommon( iparam ) )
      {
        /*
         * Dans le cas colonne, on a TEMPS : TEMPS+4 variable1 : 4+NPPOIN14+4 variablei : 4+NPPOIN14+4 pour chaque
         * variable soit: tempo= 4+4+Nbv1(4+NPPOINT4+4) d'ou
         * for column we have TEMPS: TEMPS+4 variable1 : 4+NPPOIN14+4 variablei : 4+NPPOIN14+4
         * for each variable wich means: tempo= 4+4+Nbv1(4+NPPOINT4+4)
         */
        tempo = 12 + 4 * nbv1 * nbPtOrElt + 8 * nbv1;
      }
      else
      {
        /*
         * Temps,variable1,variablei,... soit 4+(NPPOIN14)Nbv1+4
         * Temps,variable1,variablei,... means 4+(NPPOIN14)Nbv1+4
         */
        tempo = 12 + 4 * nbv1 * nbPtOrElt;
      }

      // nb d'octets restants a lire divise par nb octets par pas de temps.
      // number of remaining bytes to read divided by the number of all bytes in timestep
      final int nbPasTempsEstime = (int)(m_helper.getAvailable() / tempo);

      // le readRecord est deja fait.
      // the read record is already done

      // tempo stocke le nombre de pas de temps reellement lu.
      // tempo stores the number of timesteps really readed
      // int nbPasTemps = 0;
      if( nbv1 > 0 )
      {
        m_helper.readSequentialData();
        final FileChannel ch = m_helper.getChannel();
        if( m_onlyReadLast )
        {
          ch.position( ch.position() + tempo * (nbPasTempsEstime - 1) );
        }
        else if( isReadOnlyTimeStep() )
        {
          ch.position( ch.position() + (tempo * (this.readTimeStepFrom_)) );
        }
        // on va lire que les valeurs des pas de temps.
        int iCountStep = 0;
        int iCountVariable = 0;
        int iCountValue = 0;
        Map<String, List<Double>> mapResStep = new HashMap<>();
        List<Double> listVarVal = new ArrayList<>();
        m_helper.readAll( 4 );
        bf = m_helper.getBuffer();
        Float stepTime = bf.getFloat();
        m_startDateTime = stepTime.longValue();
        skipRecords( 2 );
        while( true )
        {
          if( iCountValue == m_listPoses.size() )
          {
            mapResStep.put( m_namesVariables[iCountVariable], listVarVal );
            iCountValue = 0;
            iCountVariable++;
            listVarVal = new ArrayList<>();
            skipRecords( 2 );
          }
          if( iCountVariable == m_namesVariables.length )
          {
            System.out.println( "put: " + stepTime );
            m_resMap.put( iCountStep, new Pair<>( "" + stepTime, mapResStep ) ); //$NON-NLS-1$
            iCountStep++;
            mapResStep = new HashMap<>();
            iCountVariable = 0;
            if( m_helper.getAvailable() > 0 )
            {
              m_helper.readAll( 4 );
              bf = m_helper.getBuffer();
              stepTime = bf.getFloat();
              skipRecords( 2 );
            }
          }
          if( m_helper.getAvailable() <= 0 )
          {
            break;
          }
          // TEMPS
          // utilisation de la variable temporaire
          // on lit un float sur 4 bytes

          // use of temporary variable, we read floats( 4 bytes )
          m_helper.readAll( 4 );
          bf = m_helper.getBuffer();
          listVarVal.add( readFloatingPointValue( bf ) );
          iCountValue++;
        }
      }
      if( !isReadOnlyTimeStep() )
      {
        // le type du maillage peut etre different si on a à traiter d'un cas 3D.
        // dans ce cas, il y a 6 noeuds par elements (2 triangles formant un truc 3D)

        // the type of mesh can be different if we have to deal with 3D
        // in this case there are 6 nodes per elements( 2 triangles wich shape 3D )
//        EfElementType type = EfElementType.getCommunType( m_trianglesNodes[0].getPtNb() );
//        if( type == EfElementType.T6 )
//          type = EfElementType.T3_FOR_3D;
//        inter.setMaillage( new EfGridArray( points, m_trianglesNodes, type ) );
      }

    }
//    catch( final IOException e )
//    {
////      analyze_.manageException( e );
//    }
    finally
    {
      closeHelper();
    }
    return m_resMap;
  }

  public void closeHelper( )
  {
    if( m_helper != null )
    {
      try
      {
        m_helper.getChannel().close();
        m_helper = null;
      }
      catch( final IOException e1 )
      {
        // TODO: log it..
      }
    }
  }

  private void skipRecords( final int amount )
  {
    for( int i = 0; i < amount; ++i )
    {
      try
      {
        m_helper.readAll( 4 );
      }
      catch( IOException e )
      {
        e.printStackTrace();
      }
      ByteBuffer bf = m_helper.getBuffer();
//      float record = 
      bf.getInt();
    }
  }

  private Double readFloatingPointValue( ByteBuffer bf )
  {
    if( m_precisionDouble )
    {
      return bf.getDouble();
    }
    else
    {
      return (double)bf.getFloat();
    }
  }

  public void setFile( final File f ) throws FileNotFoundException
  {
    m_file = f;
    m_in = new FileInputStream( f );
  }

  public long getReadTimeStepFrom( )
  {
    return readTimeStepFrom_;
  }

  public boolean isReadOnlyTimeStep( )
  {
    return readTimeStepFrom_ >= 0;
  }

  /**
   * @param _readTimeStepFrom
   *          si diff de -1, seul les pas de temps seront lu et cela à partir du pas de temps donnée
   */
  public void setReadTimeStepFrom( final int _readTimeStepFrom )
  {
    readTimeStepFrom_ = _readTimeStepFrom;
  }

  public void setModelElementHandler( NodeResultsHandler handler )
  {
    m_nodesHandler = handler;

  }

  public void feedHandler( final Integer index )
  {
    Map<VAR_NAMES, String> varNames = resolveVarNames();
    Map<String, List<Double>> actResMap = getResult( index );
    List<Double> listBot = actResMap.get( varNames.get( VAR_NAMES.BOTTOM ) );
    List<Double> listVelU = actResMap.get( varNames.get( VAR_NAMES.VEL_U ) );
    List<Double> listVelV = actResMap.get( varNames.get( VAR_NAMES.VEL_V ) );
    List<Double> listWD = actResMap.get( varNames.get( VAR_NAMES.WD ) );
    List<Double> listWL = actResMap.get( varNames.get( VAR_NAMES.WL ) );
    for( int i = 0; i < m_listPoses.size(); ++i )
    {
      GM_Position pos = m_listPoses.get( i );
      m_nodesHandler.handleNode( "", i, pos.getX(), pos.getY(), listBot.get( i ) ); //$NON-NLS-1$
      m_nodesHandler.handleResult( "", i, listVelU.get( i ), listVelV.get( i ), listWD.get( i ), listWL.get( i ) ); //$NON-NLS-1$
    }
  }

  public void feedHandlerWithTriangles( )
  {
    for( int i = 0; i < m_trianglesNodes.size(); ++i )
    {
      int[] nodesIds = m_trianglesNodes.get( i );
      m_nodesHandler.createTrianglesByIds( nodesIds );
    }
  }

  private Map<VAR_NAMES, String> resolveVarNames( )
  {
    Map<VAR_NAMES, String> names = Collections.synchronizedMap( new EnumMap<VAR_NAMES, String>( VAR_NAMES.class ) );
    for( int i = 0; i < m_namesVariables.length; ++i )
    {
      switch( m_namesVariables[i].toUpperCase() )
      {
        case BOTTOM_En:
        case BOTTOM_Fr:
          names.put( VAR_NAMES.BOTTOM, m_namesVariables[i] );
          break;
        case VEL_U_En:
        case VEL_U_Fr:
          names.put( VAR_NAMES.VEL_U, m_namesVariables[i] );
          break;
        case VEL_V_En:
        case VEL_V_Fr:
          names.put( VAR_NAMES.VEL_V, m_namesVariables[i] );
          break;
        case WD_En:
        case WD_Fr:
          names.put( VAR_NAMES.WD, m_namesVariables[i] );
          break;
        case WL_En:
        case WL_Fr:
          names.put( VAR_NAMES.WL, m_namesVariables[i] );
          break;

        default:
          break;
      }
    }
    return names;
  }

//  private Map<String, List<Double>> getResultForTimeStep( final Date stepDate )
//  {
//    System.out.println("get: " + stepDate);
//    if( stepDate == null )
//    {
//      if( m_iterTimes == null )
//      {
//        m_iterTimes = m_resMap.keySet().iterator();
//      }
//      if( m_iterTimes.hasNext() )
//      {
//        return m_resMap.get( m_iterTimes.next() );
//      }
//    }
//    else
//    {
//      return m_resMap.get( stepDate.getTime() );
//    }
//    return null;
//  }

  private Map<String, List<Double>> getResult( final Integer index )
  {
    System.out.println( "get index: " + index );
    if( index == null )
    {
      if( m_index == null )
      {
        m_index = 0;
      }
      if( m_index < m_resMap.size() )
      {
        return m_resMap.get( m_index++ ).second;
      }
    }
    else
    {
      Pair<String, Map<String, List<Double>>> pair = m_resMap.get( index );
      return pair.second;
    }
    return null;
  }
}