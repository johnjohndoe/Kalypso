package view;

import java.awt.BorderLayout;
import java.awt.FlowLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.math.BigDecimal;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.Vector;

import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JInternalFrame;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.JToolBar;

import org.kalypsodeegree_impl.model.cv.RectifiedGridCoverage;

import tools.Number;
import converter.ArcGridConverter;
import damageAnalysis.DamageAnalysis;
import damageAnalysis.DamageAnalysisContext;
import damageAnalysis.DataModel;

/**
 * View for displaying the statistic values like sum, minValue and maxValue for
 * a choosen grid of the damageDirectory, the values are calculated for each
 * landuseType and also for each administrationUnit, if an
 * administrationUnitGrid is used
 * 
 * @author N. Peiler
 *  
 */
public class StatisticView extends JInternalFrame implements ActionListener {

	/**
	 * the MenuHandler of the FloodRiskAnalysisView
	 */
	private MenuHandler menuHandler = null;

	/**
	 * comboBox with AsciiGrid-Files, which are stored in the damageDirectory of
	 * the project
	 */
	JComboBox cb_grids = null;

	JTextArea textArea = null;

	JScrollPane scroller = null;

	JButton b_clear = null;

	/**
	 * the actual landuseGrid of the project
	 */
	RectifiedGridCoverage landuseGrid = null;

	/**
	 * the actual administrationUnitGrid of the project
	 */
	RectifiedGridCoverage administrationUnitGrid = null;

	/**
	 * constructs a StatisticView
	 * 
	 * @param menuHandler
	 */
	public StatisticView(MenuHandler menuHandler) {
		super("StatisticView", true, true, true, true);
		setBounds(0, 0, 500, 300);
		this.menuHandler = menuHandler;
		File damageDir = new File(menuHandler.workingDir + "\\Damage");
		initGUI(damageDir);
		show();
	}

	private void initGUI(File damageDir) {
		//TopToolBar
		File[] grids = damageDir.listFiles(new GridFilter());
		cb_grids = new JComboBox(grids);
		cb_grids.setRenderer(new GridCellRenderer());
		cb_grids.addActionListener(this);
		cb_grids.setActionCommand("cb_grids changed");
		JToolBar topToolBar = new JToolBar();
		topToolBar.add(cb_grids);

		//BottomToolBar
		b_clear = new JButton("Clear");
		b_clear.addActionListener(this);
		JToolBar bottomToolBar = new JToolBar();
		bottomToolBar.setLayout(new FlowLayout(FlowLayout.CENTER, 10, 5));
		bottomToolBar.add(b_clear);

		//TextArea
		textArea = new JTextArea();
		scroller = new JScrollPane();
		textArea.setText("");
		scroller.setViewportView(textArea);

		getContentPane().setLayout(new BorderLayout());
		getContentPane().add(topToolBar, BorderLayout.NORTH);
		getContentPane().add(bottomToolBar, BorderLayout.SOUTH);
		getContentPane().add(scroller, BorderLayout.CENTER);
	}

	public void actionPerformed(ActionEvent e) {
		String action = e.getActionCommand();
		if ("cb_grids changed".equals(action)) {
			int index = cb_grids.getItemCount();
			if (index > 0) {
				new Thread() {
					public void run() {
						File gridFile = (File) cb_grids.getSelectedItem();
						ArcGridConverter gridConverter = new ArcGridConverter();
						RectifiedGridCoverage gridCoverage = gridConverter
								.importGridArc(gridFile);
						DamageAnalysisContext damageAnalysisContext = menuHandler.damageAnalysisContext;

						logln("Statistics for "
								+ ((File) cb_grids.getSelectedItem()).getName()
								+ "...");
						if (damageAnalysisContext.getAdministrationUnitList() != null) {
							if (landuseGrid == null
									&& administrationUnitGrid == null) {
								File landuseDataModelGML = new File(
										menuHandler.workingDir
												+ "\\Landuse\\LanduseDataModel.gml");
								File landuseDataModelSchema = new File(
										menuHandler.schemaDir
												+ "\\LanduseDataModel.xsd");
								File administrationUnitDataModelGML = new File(
										menuHandler.workingDir
												+ "\\AdministrationUnit\\AdministrationUnitDataModel.gml");
								File administrationUnitDataModelSchema = new File(
										menuHandler.schemaDir
												+ "\\AdministrationUnitDataModel.xsd");
								try {
									landuseGrid = DataModel.createLanduseGrid(
											landuseDataModelGML,
											landuseDataModelSchema);
									administrationUnitGrid = DataModel
											.createAdministrationUnitGrid(
													administrationUnitDataModelGML,
													administrationUnitDataModelSchema);
								} catch (Exception e) {
									System.out.println(e);
								}
							}
							getStatistics(gridCoverage, landuseGrid,
									damageAnalysisContext.getLanduseTypeList(),
									administrationUnitGrid,
									damageAnalysisContext
											.getAdministrationUnitList());
						} else {
							if (landuseGrid == null) {
								File landuseDataModelGML = new File(
										menuHandler.workingDir
												+ "\\Landuse\\LanduseDataModel.gml");
								File landuseDataModelSchema = new File(
										menuHandler.schemaDir
												+ "\\LanduseDataModel.xsd");
								try {
									landuseGrid = DataModel.createLanduseGrid(
											landuseDataModelGML,
											landuseDataModelSchema);
								} catch (Exception e) {
									System.out.println(e);
								}
							}
							getStatistics( gridCoverage, landuseGrid, damageAnalysisContext.getLanduseTypeList() );
            }
					}
				}.start();
			}
		}

		if ("Clear".equals(action)) {
			textArea.setText("");
		}
	}

	/**
	 * writes the statistic values like sum, minValue and maxValue for a choosen
	 * damageGrid in the textArea, the values are calculated for each
	 * landuseType
	 * 
	 * @param damageGrid
	 * @param landuseGrid
	 * @param landuseTypeList
	 */
	void getStatistics(RectifiedGridCoverage damageGrid,
			RectifiedGridCoverage landuseGrid, Hashtable landuseTypeList) {
		Hashtable statistics = null;
    try
    {
      statistics = DamageAnalysis.getStatistics(damageGrid,
      		landuseGrid);
    }
    catch( Exception e )
    {
      e.printStackTrace();
    }
    Iterator it = statistics.keySet().iterator();
		while (it.hasNext()) {
			Integer key = (Integer) it.next();
			Vector statisticsVector = (Vector) statistics.get(key);
			Double sum = (Double) statisticsVector.get(0);
			Double min = (Double) statisticsVector.get(1);
			Double max = (Double) statisticsVector.get(2);
			Iterator iterator = landuseTypeList.keySet().iterator();
			String landuse = null;
			while (iterator.hasNext()) {
				landuse = (String) iterator.next();
				Integer actualKey = (Integer) landuseTypeList.get(landuse);
				if (actualKey.equals(key)) {
					break;
				}
			}
			int mode = BigDecimal.ROUND_HALF_EVEN;
			logln(landuse + ": Sum=" + Number.round(sum.doubleValue(), 2, mode)
					+ ", MinValue=" + Number.round(min.doubleValue(), 4, mode)
					+ ", MaxValue=" + Number.round(max.doubleValue(), 4, mode));
		}
	}

	/**
	 * writes the statistic values like sum, minValue and maxValue for a choosen
	 * damageGrid in the textArea, the values are calculated for each
	 * landuseType and administrationUnit
	 * 
	 * @param damageGrid
	 * @param landuseGrid
	 * @param landuseTypeList
	 * @param administrationUnitGrid
	 * @param administrationUnitList
	 */
	void getStatistics(RectifiedGridCoverage damageGrid,
			RectifiedGridCoverage landuseGrid, Hashtable landuseTypeList,
			RectifiedGridCoverage administrationUnitGrid,
			Hashtable administrationUnitList) {

		Hashtable statistics = null;
    try
    {
      statistics = DamageAnalysis.getStatistics(damageGrid,
      		landuseGrid, administrationUnitGrid);
    }
    catch( Exception e )
    {
      e.printStackTrace();
    }
    Iterator it = statistics.keySet().iterator();
		while (it.hasNext()) {
			Integer administrationUnitKey = (Integer) it.next();
			Hashtable statistics_landuse = (Hashtable) statistics
					.get(administrationUnitKey);
			Iterator it1 = statistics_landuse.keySet().iterator();
			double sum_adminUnit = 0;
			Iterator iter = administrationUnitList.keySet().iterator();
			String adminUnit = null;
			while (iter.hasNext()) {
				adminUnit = (String) iter.next();
				Integer actualKey = (Integer) administrationUnitList
						.get(adminUnit);
				if (actualKey.equals(administrationUnitKey)) {
					break;
				}
			}
			logln(adminUnit + ": ");
			while (it1.hasNext()) {
				Integer landuseKey = (Integer) it1.next();
				Vector statisticsVector = (Vector) statistics_landuse
						.get(landuseKey);
				Double sum = (Double) statisticsVector.get(0);
				sum_adminUnit = sum_adminUnit + sum.doubleValue();
				Double min = (Double) statisticsVector.get(1);
				Double max = (Double) statisticsVector.get(2);
				Iterator iterator = landuseTypeList.keySet().iterator();
				String landuse = null;
				while (iterator.hasNext()) {
					landuse = (String) iterator.next();
					Integer actualKey = (Integer) landuseTypeList.get(landuse);
					if (actualKey.equals(landuseKey)) {
						break;
					}
				}
				int mode = BigDecimal.ROUND_HALF_EVEN;
				logln(landuse + ": Sum="
						+ Number.round(sum.doubleValue(), 2, mode)
						+ ", MinValue="
						+ Number.round(min.doubleValue(), 4, mode)
						+ ", MaxValue="
						+ Number.round(max.doubleValue(), 4, mode));
			}
			int mode = BigDecimal.ROUND_HALF_EVEN;
			logln("Summed Damage=" + Number.round(sum_adminUnit, 2, mode)
					+ "\n");
		}
	}

	void logln(String text) {
		if (text != null) {
			System.out.println(text);

			if (textArea.getRows() > 10)
				textArea.setRows(10);

			textArea.append(text + "\n");
			try {
				//always scroll to the end of the document in the textArea
				textArea.setCaretPosition(textArea.getDocument().getLength());
			} catch (Exception e) {
				System.out.println(e);
			}
			repaint();
		}
	}

}