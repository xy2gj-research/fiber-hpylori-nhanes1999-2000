# Regenerate Figure 1: participant flowchart (300+ DPI, RGB, with ML branch)
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
from matplotlib.patches import FancyBboxPatch, FancyArrowPatch

plt.rcParams["font.family"] = "Arial"
OUT = r"E:/hp/研究内容/图表/第二版/"

fig, ax = plt.subplots(figsize=(8.0, 6.2))
ax.set_xlim(0, 10); ax.set_ylim(0, 10); ax.axis("off")

def box(x, y, w, h, text, fc="white", fs=8.5, bold=False):
    b = FancyBboxPatch((x, y), w, h, boxstyle="round,pad=0.06",
                       fc=fc, ec="black", lw=0.9)
    ax.add_patch(b)
    ax.text(x + w/2, y + h/2, text, ha="center", va="center", fontsize=fs,
            fontweight="bold" if bold else "normal", linespacing=1.25)

def arrow(x1, y1, x2, y2, dashed=False):
    ax.add_patch(FancyArrowPatch((x1, y1), (x2, y2), arrowstyle="-|>",
                                 mutation_scale=12, lw=0.9,
                                 linestyle="--" if dashed else "-", color="black"))

# main flow (left column)
box(0.4, 8.9, 4.2, 0.85, "Participants in NHANES 1999\u20132000\n(n = 9,965)")
box(0.4, 7.3, 4.2, 0.85, "Adult participants (\u2265 20 years)\n(n = 4,880)")
box(0.4, 5.7, 4.2, 0.85, "Participants with valid H. pylori serostatus\n(n = 4,037)")
box(0.4, 4.1, 4.2, 0.85, "Participants with dietary recall data\n(n = 3,898)")
box(0.4, 2.5, 4.2, 0.85, "Final analytic sample\n(n = 3,340)", fc="#DCE9F5", bold=True)
box(0.4, 0.7, 4.2, 0.95, "Survey-weighted regression analyses\n(n = 3,295 with day-1 dietary weights)", fc="#DCE9F5", fs=8)

# exclusion boxes (right column)
box(5.6, 8.9, 4.0, 0.85, "Excluded (n = 5,085):\n\u2022 Age < 20 years", fs=8)
box(5.6, 7.3, 4.0, 0.85, "Excluded (n = 843):\n\u2022 Missing serology (n = 735)\n\u2022 Equivocal serology (n = 108)", fs=8)
box(5.6, 5.7, 4.0, 0.85, "Excluded (n = 139):\n\u2022 Missing dietary fiber data", fs=8)
box(5.6, 4.1, 4.0, 0.85, "Excluded (n = 558):\n\u2022 Missing covariates (BMI, PIR,\n   smoking, energy)", fs=8)
box(5.6, 0.7, 4.0, 0.95, "Exploratory machine learning subset\n(n = 2,100 with complete data\non all 14 predictors)", fc="#F5EEDC", fs=8)

# arrows
arrow(4.6, 9.32, 5.6, 9.32, dashed=True)
arrow(2.5, 8.9, 2.5, 8.15)
arrow(4.6, 7.72, 5.6, 7.72, dashed=True)
arrow(2.5, 7.3, 2.5, 6.55)
arrow(4.6, 6.12, 5.6, 6.12, dashed=True)
arrow(2.5, 5.7, 2.5, 4.95)
arrow(4.6, 4.52, 5.6, 4.52, dashed=True)
arrow(2.5, 4.1, 2.5, 3.35)
arrow(2.5, 2.5, 2.5, 1.65)
arrow(4.6, 2.92, 7.6, 2.92, dashed=True)
ax.add_patch(FancyArrowPatch((7.6, 2.92), (7.6, 1.65), arrowstyle="-|>",
                             mutation_scale=12, lw=0.9, linestyle="--", color="black"))

fig.tight_layout()
fig.savefig(OUT + "Figure1_Flowchart_v5.tif", dpi=600, format="tiff",
            pil_kwargs={"compression": "tiff_lzw"})
plt.close(fig)
print("Figure1 saved")
